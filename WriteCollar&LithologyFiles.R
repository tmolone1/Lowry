rm(list-ls())
library(dplyr)
library(readr)
library(readxl)
list.files()
compiled<-read_excel("Survey Data.xlsx",sheet="COMPILED")
lith<-read_excel("Copy of LeapfrogInput-MK (3).xlsx", sheet= "Lithology")
write_csv(compiled, "collar.csv")
lith<-lith[,c(1:6)]
colnames(lith)[5]<-"2nd_lithology"
colnames(lith)[6]<-"comment"
write_csv(lith,"Lithology.csv")