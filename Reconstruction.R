library(dplyr)
library(reshape)
library(ggplot2)
library(reshape2)

setwd("/Users/Tony")

setwd("/Users/Heng/Documents/CornellWork/MPS_Project/GIT")


list.files()


theDataset <- read.csv(file = "data.csv", header = TRUE, sep = "," )


validUsers <- filter(theDataset, uuid != "", ua != "")
byUuid <- group_by(validUsers, uuid)
filteredUsers <- filter(byUuid, n()<40)

filteredUsers <- filteredUsers[order(filteredUsers$uuid,filteredUsers$ts),]


singleUser2 <- filter(validUsers,uuid == "1141dc836b73b8279207296550f6b342")
# validUsers <- head(validUsers, 100)
mytable <- table(filteredUsers$uuid)
#which.max(mytable)
# names(which.max(table(validUsers$uuid)))
# names(mytable>60)
freqDF = as.data.frame(mytable)

colnames(freqDF)=c("uuid","freq")
freqDF = filter(freqDF,uuid != "",freq!=0)

idCol = rep(0, length(filteredUsers$uuid))
rowCount = 0
for (i in 1:length(freqDF$freq)){
  idCol[(rowCount+1) : (rowCount + freqDF$freq[i])] <- c(1:freqDF$freq[i])
  rowCount = rowCount + freqDF$freq[i]
}
filteredUsers <- as.data.frame(filteredUsers)
filteredUsers <- cbind(id = idCol, filteredUsers)

#plot(mytable)
#quantile(mytable, c(.25, .50, .95))
# df=as.data.frame(mytable)
# uuidCount = length(df[,1])

ptm <- proc.time()
filteredUsers1000 <- filteredUsers
valuePair = cbind(id = filteredUsers1000$id, uuid = filteredUsers1000$uuid, sttp = filteredUsers1000$sttp, ts = filteredUsers1000$ts)
#valuePair = t(valuePair)
colnames(valuePair)
df = as.data.frame(valuePair)
casted <- dcast(df,  id ~ uuid, value.var = "sttp", na.rm = TRUE)
write.csv(casted, file = "testData.csv")
proc.time() - ptm


# actionTable <- matrix(rep(0),nrow = 43,ncol = uuidCount)
# for (i in 1:length(validUsers[,1])){
#   #if uuid exists in actionTable
#   index = match(validUsers$uuid[i],actionTable[1,])
#   if (!is.na(index)){
#     actionTable[match(0,actionTable[,index]),index] <- validUsers$sttp[i]
#   }else{
#     newEntryIndex = match(0,actionTable[1,])
#     print(validUsers$uuid[i])
#     actionTable[1,newEntryIndex] <- validUsers$uuid[i]#add uuid to new entry
#     print(validUsers$ua[i])
#     actionTable[2,newEntryIndex] <- validUsers$ua[i]#add user agent to new entry
#     actionTable[3,newEntryIndex] <- validUsers$ts[i]#add initial ts to new entry
#     actionTable[4,newEntryIndex] <- -1#add -1 to final ts of the new entry
#     if(i>1){
#       actionTable[4,newEntryIndex-1] <- validUsers$ts[i-1]#add last ts to the last entry
#     }
#     actionTable[5,newEntryIndex] <- validUsers$sttp[i]#add sttp to new entry
#   }
# }
# actionTable[4,length(actionTable[1,])] <- validUsers$ts[length(validUsers[,1])]


