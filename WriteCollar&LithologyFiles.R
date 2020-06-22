rm(list-ls())
library(dplyr)
library(readr)
library(readxl)
library(sp)
library(rgdal)
list.files()
compiled<-read_excel("Survey Data.xlsx",sheet="COMPILED")
lith<-read_excel("Copy of LeapfrogInput-MK (3) _ TM edit.xlsx", sheet= "Lithology")

#define mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#summarise each character variable as the mode, most frequently ocurring value
chars<-dplyr::select_if(compiled, is.character)
chars<- chars %>%
  group_by(LOCATION) %>%
  summarize_each(funs(Mode))

#summarize each numeric variable as the mean of all records for that location
#this is instead of taking most recent year surveyed.  
# some points have multiple records from same year, e.g. BG-MW01, has separate records GRND, TOC, and PVC survey all in 2005
# XYs for each are subtlely different.  How do we decide which XY to use?  
# if we take mean of everything it doesnt matter.
# if client wants this treated differently, should provide revised coordinate file.
nums<-dplyr::select_if(compiled, is.numeric)
nums<-as_tibble(cbind(compiled$LOCATION,nums))
colnames(nums)[1]<-"LOCATION"
nums<- nums %>%
  group_by(LOCATION) %>%
  summarize_each(funs(mean))

collar<-merge(chars, nums, by="LOCATION")

#create table to identify unique lithologies and assign which are mapped to coarse in fine_coarse variable
v<-rep("Fine",length(unique(lith$Valid_Lith)))
v[c(2,5,7)] <- "Coarse"  # check this and edit it if necessary based on geology
df<-data.frame(cbind(unique(lith$Valid_Lith),v))
lith$fine_coarse <- "Fine"
lith[lith$Valid_Lith %in% df$V1[df$v=="Coarse"],"fine_coarse"] <- "Coarse"

#within the hi-res area, add a 1 foot fine interval at the end of each push (refusal)
hi_res<-collar[collar[11]>1646716 & collar[11]<1647404 & collar[12] > 3258521 & collar[12] < 3259121,"LOCATION"]
lith_mod<-lith[lith$`Hole ID` %in% hi_res,]
refusals<-lith_mod %>%
  group_by(`Hole ID`) %>%
  summarize(From=max(To),To=max(To)+1,Lithology=NA,'2nd lithology'=NA,comment=NA,Valid_Lith="Termination/Refusal",fine_coarse="Fine")
lith_mod<-rbind(lith_mod,refusals)
lith_mod <- arrange(lith_mod, desc(lith_mod$`Hole ID`))

write_csv(collar, "collar.csv")
write_csv(lith,"Lithology.csv")
write_csv(lith_mod,"Lithology_hi_res.csv")
Wells_points <- SpatialPointsDataFrame(collar[,c(12,11)], 
                                       data= collar,
                                       proj4string = CRS("+init=ESRI:102654")) # colorado state plane central NAD83
# # use this to write a shapefile of the wells if desired:
# writeOGR(Wells_points, dsn="M:/ItoN/MidnightSunTrinityJV/Modeling/Shapefiles", layer = "Wells_points", driver = "ESRI Shapefile", overwrite_layer=TRUE)
