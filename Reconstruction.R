library(dplyr)
library(reshape)
library(ggplot2)

#setwd("/Users/Tony")

setwd("/Users/Heng/Documents/CornellWork/MPS_Project/GIT")


list.files()

theDataset <- read.csv(file = "data.csv", header = TRUE, sep = ",")

validUsers <- filter(theDataset, uuid != "", ua != "")
validUsers <- validUsers[order(validUsers$ip,validUsers$uuid,validUsers$ts),]
#singleUser <- filter(theDataset,uuid == "34589c40d81bc8bd6c9ca0ebe0795d0d")
mytable <- table(validUsers$uuid)

#which.max(mytable)
#names(which.max(table(validUsers$uuid)))

#duuid =density(table(validUsers$uuid))
#plot(duuid)

#plot(mytable)
#quantile(mytable, c(.25, .50, .95))
df=as.data.frame(mytable)
uuidCount = length(df[,1])


actionTable <- matrix(rep(0),nrow = 43,ncol = uuidCount)
for (i in 1:length(validUsers[,1])){
  #if uuid exists in actionTable
  index = match(validUsers$uuid[i],actionTable[1,])
  if (!is.na(index)){
    actionTable[match(0,actionTable[,index]),index] <- validUsers$sttp[i]
  }else{
    newEntryIndex = match(0,actionTable[1,]);
    actionTable[1,newEntryIndex] <- validUsers$uuid[i]#add uuid to new entry
    actionTable[2,newEntryIndex] <- validUsers$ua[i]#add user agent to new entry
    actionTable[3,newEntryIndex] <- validUsers$ts[i]#add initial ts to new entry
    actionTable[4,newEntryIndex] <- -1#add -1 to final ts of the new entry
    actionTable[4,newEntryIndex-1] <- validUsers$ts[i-1]#add last ts to the last entry
    actionTable[5,newEntryIndex] <- validUsers$sttp[i]#add sttp to new entry
  }
}
actionTable[4,length(actionTable[1,])] <- validUsers$ts[length(validUsers[,1])]
