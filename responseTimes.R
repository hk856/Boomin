# import and manipulate the dataset

theDataset <- read.csv(file = "newData10.csv", header = TRUE, sep = ",")
theDataset <- cbind(theDataset,rep(0,length(theDataset[,1])))
colnames(theDataset)[31:32] <- c("rt")

# order the dataset according to the uuid and when it is missing use the ip.

theDataset$uuid <- as.character(theDataset$uuid)
theDataset$ip <- as.character(theDataset$ip)
sbDataset <- theDataset[mixedorder(theDataset$uuid),]
noUuid <- which(sbDataset$uuid == "")
ipDataset <- sbDataset[noUuid,]
ipDataset2 <- ipDataset[mixedorder(ipDataset$ip),]
sbDataset[noUuid,] <- ipDataset2
sbDataset[noUuid,]$uuid <- sbDataset[noUuid,]$ip

# define the time threshold after which a new session is starting
threshold <- 600 

# calculate the reaction time

noItems <- length(sbDataset[,1])
changeIndex <- which(sbDataset[2:noItems,]$uuid != sbDataset[1:(noItems - 1),]$uuid)
changeIndex <- changeIndex + 1
sbDataset$rt <- append(0,(sbDataset[2:noItems,]$ts - sbDataset[1:(noItems-1),]$ts))
sbDataset[changeIndex,]$rt <- rep(0,length(changeIndex))
newSessions <-  which(sbDataset$rt >= threshold)
sbDataset[changeIndex,]$rt <- rep(0,length(newSessions))
