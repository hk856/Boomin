library(dplyr)
library(reshape)
library(ggplot2)


setwd("/Users/Tony")

setwd("/Users/Heng/Documents/CornellWork/MPS_Project/GIT")


list.files()

theDataset <- read.csv(file = "data.csv", header = TRUE, sep = ",")

validUsers <- filter(theDataset, uuid != "", ua != "")
validUsers <- validUsers[order(validUsers$ip,validUsers$uuid,validUsers$ts),]
