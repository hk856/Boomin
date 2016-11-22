library(dplyr)
library(reshape)
library(ggplot2)
library(reshape2)

setwd("/Users/Heng/Documents/CornellWork/MPS_Project/GIT")
setwd("/Users/Tony")

#read csv
theDataset <- read.csv(file = "data.csv", header = TRUE, sep = "," )

temp1 <- data.frame(grepl(pattern = "/f/|/m/|price_min|price_max|custom_range|order_by", theDataset$uri, ignore.case = T))
temp2 <- data.frame(grepl(pattern = "/f/", theDataset$uri, ignore.case = T))
temp3 <- data.frame(grepl(pattern = "/m/", theDataset$uri, ignore.case = T))
temp4 <- data.frame(grepl(pattern = "price_min|price_max", theDataset$uri, ignore.case = T))
temp5 <- data.frame(grepl(pattern = "order_by", theDataset$uri, ignore.case = T))
temp6 <- data.frame(grepl(pattern = "custom_range", theDataset$uri, ignore.case = T))
temp7 <- data.frame(grepl(pattern = "keyphrase", theDataset$uri, ignore.case = T))

theDataset <- cbind(theDataset,temp1,temp2,temp3,temp4,temp5,temp6,temp7) # used filter functi
colnames(theDataset)[13] <- "filtered"
colnames(theDataset)[14] <- "radio"
colnames(theDataset)[15] <- "checkBox"
colnames(theDataset)[16] <- "priceRange"
colnames(theDataset)[17] <- "orderBy"
colnames(theDataset)[18] <- "customRange"
colnames(theDataset)[19] <- "search"


#filter users. Only keep pc users and mobile users who are not using apps
filteredUsers <- filter(theDataset, uuid != "", ua != "")
filteredUsers[grep("ios", filteredUsers$uri), "platform"] <- "ios"
filteredUsers[grep("android", filteredUsers$uri), "platform"] <- "android"
filteredUsers = filter(filteredUsers, is.na(platform))
filteredUsers$platform = NULL
#group by uuid and keep those users who has less than 40 actions
byUuid <- group_by(filteredUsers, uuid)
filteredUsers <- filter(byUuid, n()<40)

#order the dataframe by uuid and time stamp
filteredUsers <- filteredUsers[order(filteredUsers$uuid,filteredUsers$ts),]

#find how many actions each uuid took
mytable <- table(filteredUsers$uuid)
freqDF = as.data.frame(mytable)
colnames(freqDF)=c("uuid","freq")
freqDF = filter(freqDF,uuid != "",freq!=0)

#give each action of an uuid an id
idCol = rep(0, length(filteredUsers$uuid))
rowCount = 0
for (i in 1:length(freqDF$freq)){
  idCol[(rowCount+1) : (rowCount + freqDF$freq[i])] <- c(1:freqDF$freq[i])
  rowCount = rowCount + freqDF$freq[i]
}
filteredUsers <- as.data.frame(filteredUsers)
filteredUsers <- cbind(id = idCol, filteredUsers)

#take some valuable columns out of the dataframe and cbind them into a new dataframe
valuePair = cbind(id = filteredUsers$id, uuid = filteredUsers$uuid, 
                  sttp = filteredUsers$sttp, ts = filteredUsers$ts, cid = filteredUsers$cid,
                  platform = filteredUsers$ua, filtered = filteredUsers$filtered, 
                  radio = filteredUsers$radio, checkBox = filteredUsers$checkBox, 
                  priceRange = filteredUsers$priceRange, orderBy = filteredUsers$orderBy, 
                  custom_range = filteredUsers$customRange, search = filteredUsers$search)
df = as.data.frame(valuePair)
#cast the dataframe
m <- matrix(0, ncol = 22, nrow = length(freqDF[,1]))
output = data.frame(m)

colnames(output) <- c("uuid","num_actions","sttp10","browsing_time",
                      "cid40","cid379","cid407","cid579","cid427","cid12", 
                      "cid1105","cid25","cid334","cid790",
                      "platform","num_filter","num_radio_f","num_checkBox_f",
                      "num_priceRange_f","num_orderBy_f",
                      "num_custom_range","num_search")

#fill the uuid column in output
output$uuid <- freqDF$uuid

#fill the sttp column in output
casted <- dcast(df,  id ~ uuid, value.var = "sttp")
casted <- t(casted)
l <- length(casted[,1])
casted <- casted[2:l,]
output$num_actions <- unname(apply(casted, 1, function(x) length(which(!is.na(x)))))
output$sttp10 <- unname(apply(casted, 1, function(x) length(which(x==10))))

#fill the browsing time
casted <- dcast(df,  id ~ uuid, value.var = "ts")
casted <- t(casted)
casted <- casted[2:l,]
output$browsing_time <- unname(apply(casted, 1, function(x){
  NonNAindex <- which(!is.na(x))
  lastNonNA <- max(NonNAindex)
  return(x[lastNonNA]-x[1])
}))

#platform
casted <- dcast(df,  id ~ uuid, value.var = "platform")
casted <- t(casted)
casted <- casted[2:l,]
output$platform <- unname(apply(casted, 1, function(x) x[1]))

#number of filter
casted <- dcast(df,  id ~ uuid, value.var = "filtered")
casted <- t(casted)
casted <- casted[2:l,]
output$num_filter <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of radio filter
casted <- dcast(df,  id ~ uuid, value.var = "radio")
casted <- t(casted)
casted <- casted[2:l,]
output$num_radio_f <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of check box filter
casted <- dcast(df,  id ~ uuid, value.var = "checkBox")
casted <- t(casted)
casted <- casted[2:l,]
output$num_checkBox_f <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of radio filter
casted <- dcast(df,  id ~ uuid, value.var = "radio")
casted <- t(casted)
casted <- casted[2:l,]
output$num_radio_f <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of price range filter
casted <- dcast(df,  id ~ uuid, value.var = "priceRange")
casted <- t(casted)
casted <- casted[2:l,]
output$num_priceRange_f <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of order_by filter
casted <- dcast(df,  id ~ uuid, value.var = "orderBy")
casted <- t(casted)
casted <- casted[2:l,]
output$num_orderBy_f <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of custom_range filter
casted <- dcast(df,  id ~ uuid, value.var = "custom_range")
casted <- t(casted)
casted <- casted[2:l,]
output$num_custom_range <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of search
casted <- dcast(df,  id ~ uuid, value.var = "search")
casted <- t(casted)
casted <- casted[2:l,]
output$num_search <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of cid(to catagories)
casted <- dcast(df,  id ~ uuid, value.var = "cid")
casted <- t(casted)
casted <- casted[2:l,]
output$cid40 <- unname(apply(casted, 1, function(x) length(which(x=='40'))))
output$cid379 <- unname(apply(casted, 1, function(x) length(which(x=='379'))))
output$cid407 <- unname(apply(casted, 1, function(x) length(which(x=='407'))))
output$cid579 <- unname(apply(casted, 1, function(x) length(which(x=='579'))))
output$cid427 <- unname(apply(casted, 1, function(x) length(which(x=='427'))))
output$cid12 <- unname(apply(casted, 1, function(x) length(which(x=='12'))))
output$cid1105 <- unname(apply(casted, 1, function(x) length(which(x=='1105'))))
output$cid25 <- unname(apply(casted, 1, function(x) length(which(x=='25'))))
output$cid334 <- unname(apply(casted, 1, function(x) length(which(x=='334'))))
output$cid790 <- unname(apply(casted, 1, function(x) length(which(x=='790'))))

write.csv(output, file = "functionAggregation.csv")

