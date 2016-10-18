library(dplyr)
library(reshape)
library(ggplot2)
library(reshape2)

setwd("/Users/Tony")

setwd("/Users/Heng/Documents/CornellWork/MPS_Project/GIT")


list.files()

ptm <- proc.time()
theDataset <- read.csv(file = "data.csv", header = TRUE, sep = "," )


validUsers <- filter(theDataset, uuid != "", ua != "")
byUuid <- group_by(validUsers, uuid)
filteredUsers <- filter(byUuid, n()<60)

filteredUsers <- filteredUsers[order(filteredUsers$uuid,filteredUsers$ts),]
# singleUser2 <- filter(theDataset,uuid == "3cb1d0ea664017c2edfa9c4624965106")

# validUsers <- head(validUsers, 100)
# mytable <- table(validUsers$uuid)
# validUsers <- cbind(id = c(1:length(validUsers[,1])), validUsers)
#which.max(mytable)
# names(which.max(table(validUsers$uuid)))
# names(mytable>60)
# freqDF = as.data.frame(mytable)
# colnames(freqDF)=c("uuid2","freq")



#plot(mytable)
#quantile(mytable, c(.25, .50, .95))
# df=as.data.frame(mytable)
# uuidCount = length(df[,1])


valuePair = cbind(uuid = filteredUsers$uuid, sttp = filteredUsers$sttp, ts = filteredUsers$ts)
#valuePair = t(valuePair)
colnames(valuePair)
df = as.data.frame(valuePair)
casted <- dcast(df,  ts ~ uuid, value.var = "sttp", na.rm = TRUE)
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


