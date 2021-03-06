library(gtools)

# import and manipulate the dataset
setwd("/Users/Tony")
theDataset <- read.csv(file = "data.csv", header = TRUE, sep = ",")
theDataset <- cbind(theDataset,"rt"=rep(0,length(theDataset[,1])))
theDataset <- theDataset[order(theDataset$uuid,theDataset$ts),]
sbDataset <- filter(theDataset,theDataset$uuid!="")
# order the dataset according to the uuid and when it is missing use the ip.

#theDataset$uuid <- as.character(theDataset$uuid)
#theDataset$ip <- as.character(theDataset$ip)
#sbDataset <- theDataset[mixedorder(theDataset$uuid),]
#noUuid <- which(sbDataset$uuid == "")
#ipDataset <- sbDataset[noUuid,]
#ipDataset2 <- ipDataset[mixedorder(ipDataset$ip),]
#sbDataset[noUuid,] <- ipDataset2
#sbDataset[noUuid,]$uuid <- sbDataset[noUuid,]$ip

#sbDataset = theDataset
# define the time threshold after which a new session is starting
threshold <- 600

# calculate the reaction time

noItems <- length(sbDataset[,1])
changeIndex <- which(sbDataset[2:noItems,]$uuid != sbDataset[1:(noItems - 1),]$uuid)
changeIndex <- changeIndex + 1
sbDataset$rt <- append(0,(sbDataset[2:noItems,]$ts - sbDataset[1:(noItems-1),]$ts))
sbDataset[changeIndex,]$rt <- rep(0,length(changeIndex))
newSessions <-  which(sbDataset$rt >= threshold)
sbDataset[newSessions,]$rt <- rep(0,length(newSessions))


# explort the file 

write.csv(sbDataset, file = "orderedScrooge.csv")
