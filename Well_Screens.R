### Lowry Titan Missiles - Well Screens ###

# The purpose of this code is to look at the wells in the file April2020_WaterLevels.csv
# and create a new .csv that has the HoleID, From, TO, and the GW ELEV field. 

# Make sure to have your project set up with the GITHub repository for Lowry 

library(dplyr)
library(readr)


# Import data from April2020_WaterLevels.csv, Well Table 1.csv, and Lithology.csv

waterlevels <- read.csv("April2020_WaterLevels.csv")
welltable <- read.csv("Well Table 1.csv")
lithology <- read.csv("Lithology.csv")

# MW well depths

MWwells <- data.frame(matrix(ncol = 3, nrow = 15))
xmw <- c("Hole.ID", "From", "To")
colnames(MWwells) <- xmw

MWwells$Hole.ID <- welltable$ï..Well.ID

MWwells$To <- as.numeric(as.character(welltable$TDW.6.20.2016))
MWwells$From <- as.numeric(as.character(welltable$TDW.6.20.2016)) - 10


# IP and BG well depths

levels(lithology$Hole.ID)

otherwells <- lithology %>%
  group_by(Hole.ID) %>%
  summarize(
    From = max(To) - 10,
    To = max(To),
  )

#exclude DHD wells
otherwells <- otherwells[(otherwells$Hole.ID != "DHD-2-13"
                        & otherwells$Hole.ID != "DHD-2-14"
                        & otherwells$Hole.ID != "DHD-2-15"
                        & otherwells$Hole.ID != "DHD-2-16"
                        & otherwells$Hole.ID != "DHD-2-17"
                        & otherwells$Hole.ID != "DHD-2-18"
                        & otherwells$Hole.ID != "DHD-2-19"
                        & otherwells$Hole.ID != "DHD-2-20"
                        & otherwells$Hole.ID != "DHD-2-21"
                        & otherwells$Hole.ID != "DHD-2-22"
                        & otherwells$Hole.ID != "DHD-2-23"),]

#exclude MW wells as we are getting them from another file
otherwells <- otherwells[(otherwells$Hole.ID != "MW-1"
                          & otherwells$Hole.ID != "MW-2"
                          & otherwells$Hole.ID != "MW-3"
                          & otherwells$Hole.ID != "MW-4"
                          & otherwells$Hole.ID != "MW-5"
                          & otherwells$Hole.ID != "MW-6"
                          & otherwells$Hole.ID != "MW-7"
                          & otherwells$Hole.ID != "MW-8"
                          & otherwells$Hole.ID != "MW-9"
                          & otherwells$Hole.ID != "MW-10"
                          & otherwells$Hole.ID != "MW-11"
                          & otherwells$Hole.ID != "MW-12"
                          & otherwells$Hole.ID != "MW-13"
                          & otherwells$Hole.ID != "MW-14"
                          & otherwells$Hole.ID != "MW-15"),]



allwells <- rbind(otherwells, MWwells)

## the Lithology.csv is missing data for IP-21, IP-36, IP-41, and IP-50

## WARNING:  we are creating the .csv minus the 4 wells we are missing in Lithology

waterlevelsMINUS4 <- waterlevels

waterlevelsMINUS4 <- waterlevels[(waterlevels$LOCATION != "IP-21"
                          & waterlevels$LOCATION != "IP-36"
                          & waterlevels$LOCATION != "IP-41"
                          & waterlevels$LOCATION != "IP-50"),]

allwells66 <- allwells

allwells66$GW.ELEV <- waterlevelsMINUS4$GW.ELEV

# Export .csv to GITHub repository 

write.csv(allwells66, file = "Well Screens (66).csv")

