library(dplyr)
library(reshape)
library(ggplot2)

setwd("/Users/Heng/Documents/CornellWork/MPS_Project/GIT")
list.files()

theDataset <- read.csv(file = "data.csv", header = TRUE, sep = ",")
theDataset <- theDataset[order(theDataset$ip,theDataset$uuid,theDataset$ts),]

singleUser <- filter(theDataset,uuid == "104d0d7516ce00e1cd02bd2883ac8004",ip=="79.167.135.20")


