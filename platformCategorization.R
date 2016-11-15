library(dplyr)
library(reshape)
library(ggplot2)
library(reshape2)

setwd("F:/Personal/Grad School/MPS project/GIT/Data")

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
colnames(theDataset)[12] <- "filtered"
colnames(theDataset)[13] <- "radio"
colnames(theDataset)[14] <- "checkBox"
colnames(theDataset)[15] <- "priceRange"
colnames(theDataset)[16] <- "orderBy"
colnames(theDataset)[17] <- "customRange"
colnames(theDataset)[18] <- "search"

platformCate = theDataset

#general case
#mark app user in original dataset
platformCate$exist_in_ua = mapply(grepl, pattern = "skroutz", x = platformCate$ua, ignore.case = TRUE)
platformCate$exist_in_uri = mapply(grepl, pattern = "skroutz_", x = platformCate$uri, ignore.case = TRUE)

#mark mobile user in original dataset
platformCate$mobile = mapply(grepl, pattern = "mobile", x = platformCate$ua, ignore.case = TRUE)
platformCate$phone = mapply(grepl, pattern = "phone", x = platformCate$ua, ignore.case = TRUE)


#none-app user
noneAppUser = filter(platformCate,exist_in_ua == FALSE, exist_in_uri == FALSE,
                     sttp != "800", sttp != "1066", 
                     uuid != "", ua != "")

noneAppByUuid <- group_by(noneAppUser, uuid)
noneAppUser <- filter(noneAppByUuid, n()<40)

#order the dataframe by uuid and time stamp
noneAppUser <- noneAppUser[order(noneAppUser$uuid,noneAppUser$ts),]

#find how many actions each uuid took
mytable <- table(noneAppUser$uuid)
noneAppFreqDF = as.data.frame(mytable)
colnames(noneAppFreqDF)=c("uuid","freq")
noneAppFreqDF = filter(noneAppFreqDF,uuid != "",freq!=0)

#give each action of an uuid an id
idCol = rep(0, length(noneAppUser$uuid))
rowCount = 0
for (i in 1:length(noneAppFreqDF$freq)){
  idCol[(rowCount+1) : (rowCount + noneAppFreqDF$freq[i])] <- c(1:noneAppFreqDF$freq[i])
  rowCount = rowCount + noneAppFreqDF$freq[i]
}
noneAppUser <- as.data.frame(noneAppUser)
noneAppUser <- cbind(id = idCol, noneAppUser)

#take some valuable columns out of the dataframe and cbind them into a new dataframe
noneAppValuePair = cbind(id = noneAppUser$id, uuid = noneAppUser$uuid, 
                    sttp = noneAppUser$sttp, ts = noneAppUser$ts,
                    platform = noneAppUser$ua, filtered = noneAppUser$filtered, 
                    radio = noneAppUser$radio, checkBox = noneAppUser$checkBox, 
                    priceRange = noneAppUser$priceRange, orderBy = noneAppUser$orderBy, 
                    custom_range = noneAppUser$customRange, search = noneAppUser$search,
                    mobile = noneAppUser$mobile, phone = noneAppUser$phone)
df = as.data.frame(noneAppValuePair)

#cast the dataframe
m <- matrix(0, ncol = 13, nrow = length(noneAppFreqDF[,1]))
noneAppOutput = data.frame(m)

colnames(noneAppOutput) <- c("uuid","num_actions","sttp10","browsing_time",
                        "num_filter","num_radio_f","num_checkBox_f",
                        "num_priceRange_f","num_orderBy_f",
                        "num_custom_range","num_search", 
                        "is_mobile", "is_phone")

#fill the uuid column for none-app user
noneAppOutput$uuid <- noneAppFreqDF$uuid

#fill the sttp column for none-app user
casted <- dcast(df,  id ~ uuid, value.var = "sttp")
casted <- t(casted)
l <- length(casted[,1])
casted <- casted[2:l,]
noneAppOutput$num_actions <- unname(apply(casted, 1, function(x) length(which(!is.na(x)))))
noneAppOutput$sttp10 <- unname(apply(casted, 1, function(x) length(which(x==10))))


#fill the browsing time for none-app user
casted <- dcast(df,  id ~ uuid, value.var = "ts")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$browsing_time <- unname(apply(casted, 1, function(x){
  NonNAindex <- which(!is.na(x))
  lastNonNA <- max(NonNAindex)
  return(x[lastNonNA]-x[1])
}))

#number of filter
casted <- dcast(df,  id ~ uuid, value.var = "filtered")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$num_filter <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of radio filter
casted <- dcast(df,  id ~ uuid, value.var = "radio")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$num_radio_f <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of check box filter
casted <- dcast(df,  id ~ uuid, value.var = "checkBox")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$num_checkBox_f <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of price range filter
casted <- dcast(df,  id ~ uuid, value.var = "priceRange")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$num_priceRange_f <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of order_by filter
casted <- dcast(df,  id ~ uuid, value.var = "orderBy")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$num_orderBy_f <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of custom_range filter
casted <- dcast(df,  id ~ uuid, value.var = "custom_range")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$num_custom_range <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#number of search
casted <- dcast(df,  id ~ uuid, value.var = "search")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$num_search <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

#count if it's mobile or phone for future filtering out mobile user
casted <- dcast(df,  id ~ uuid, value.var = "mobile")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$is_mobile <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))

casted <- dcast(df,  id ~ uuid, value.var = "phone")
casted <- t(casted)
casted <- casted[2:l,]
noneAppOutput$is_phone <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))


#PC
#find pc user
pcUser = filter(noneAppOutput, is_mobile == 0,
                is_phone == 0)

#find distinct PC user
distinctPCUser = filter(noneAppUser, mobile == FALSE,
                        phone == FALSE) 
distinctPCUser = subset(distinctPCUser, !duplicated(ip))


#Mobile
#find mobile user
mobileUser = filter(noneAppOutput, is_mobile > 0 | is_phone > 0)

#find distinct mobile user
distinctMobileUser = filter(noneAppUser, mobile == TRUE | phone == TRUE) 
distinctMobileUser = subset(distinctMobileUser, !duplicated(ip))


#app
#find app user
appUser = filter(platformCate, exist_in_ua == TRUE | exist_in_uri == TRUE)
appUser$android = mapply(grepl, pattern = "android", x = appUser$ua, ignore.case = TRUE)
appUser$ios = mapply(grepl, pattern = "OS", x = appUser$ua, ignore.case = TRUE)


appByip <- group_by(appUser, ip)
appUser <- filter(appByip, n()<40)

#order the dataframe by ip and time stamp
appUser <- appUser[order(appUser$ip,appUser$ts),]

#find how many actions each ip took
mytable <- table(appUser$ip)
appFreqDF = as.data.frame(mytable)
colnames(appFreqDF)=c("ip","freq")
appFreqDF = filter(appFreqDF,ip != "",freq!=0)

#give each action of an ip an id
idCol = rep(0, length(appUser$ip))
rowCount = 0
for (i in 1:length(appFreqDF$freq)){
  idCol[(rowCount+1) : (rowCount + appFreqDF$freq[i])] <- c(1:appFreqDF$freq[i])
  rowCount = rowCount + appFreqDF$freq[i]
}
appUser <- as.data.frame(appUser)
appUser <- cbind(id = idCol, appUser)

#take some valuable columns out of the dataframe and cbind them into a new dataframe
appValuePair = cbind(id = appUser$id, sttp = appUser$sttp,
                     ip = appUser$ip, ts = appUser$ts, 
                     platform = appUser$ua, android = appUser$android,
                     ios = appUser$ios)
df = as.data.frame(appValuePair)

#cast the dataframe
m <- matrix(0, ncol = 7, nrow = length(appFreqDF[,1]))
appOutput = data.frame(m)

colnames(appOutput) <- c("ip","num_actions","sttp10","browsing_time","platform", "is_android", "is_ios")

#fill the ip column for app user
appOutput$ip <- appFreqDF$ip

#fill the sttp column for app user
casted <- dcast(df,  id ~ ip, value.var = "sttp")
casted <- t(casted)
l <- length(casted[,1])
casted <- casted[2:l,]
appOutput$num_actions <- unname(apply(casted, 1, function(x) length(which(!is.na(x)))))
appOutput$sttp10 <- unname(apply(casted, 1, function(x) length(which(x==10))))

#fill the browsing time for app user
casted <- dcast(df,  id ~ ip, value.var = "ts")
casted <- t(casted)
casted <- casted[2:l,]
appOutput$browsing_time <- unname(apply(casted, 1, function(x){
  NonNAindex <- which(!is.na(x))
  lastNonNA <- max(NonNAindex)
  return(x[lastNonNA]-x[1])
}))

#fill the platform
casted <- dcast(df,  id ~ ip, value.var = "platform")
casted <- t(casted)
casted <- casted[2:l,]
appOutput$platform <- unname(apply(casted, 1, function(x) x[1]))

#count if it's mobile or phone for future filtering out mobile user
casted <- dcast(df,  id ~ ip, value.var = "android")
casted <- t(casted)
casted <- casted[2:l,]
appOutput$is_android <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))


casted <- dcast(df,  id ~ ip, value.var = "ios")
casted <- t(casted)
casted <- casted[2:l,]
appOutput$is_ios <- unname(apply(casted, 1, function(x) length(which(x==TRUE))))




#ios app
#find ios user
#ua of android user might contain ios keyword but must contain android keyword.
#therefore set is_android = 0 will return all ios session
iosUser = filter(appOutput, is_ios > 0, is_android == 0)


#android app 
#find android user
#ua of android user might contain ios keyword but must contain android keyword.
#therefore set is_android > 0 will return all android session
androidUser = filter(appOutput, is_android > 0)

#combine the result of four catogories 
pc = c('num_session'= nrow(pcUser), 'distinct_users' = nrow(distinctPCUser), 
       'avg_browsing_time' = mean(pcUser$browsing_time),
       'browsing_time_per_action' = sum(pcUser$browsing_time)/ sum(pcUser$num_actions),
       'avg_num_actions' = mean(pcUser$num_actions), 
       'avg_sttp10' = mean(pcUser$sttp10), 
       'sttp10_per_action' = sum(pcUser$sttp10)/ sum(pcUser$num_actions),
       'avg_num_search' = mean(pcUser$num_search), 
       'avg_num_filter' = mean(pcUser$num_filter))

Mobile_NoneApp = c('num_session'= nrow(mobileUser), 
                   'distinct_users' = nrow(distinctMobileUser), 
                   'avg_browsing_time' = mean(mobileUser$browsing_time),
                   'browsing_time_per_action' = sum(mobileUser$browsing_time)/ sum(mobileUser$num_actions),
                   'avg_num_actions' = mean(mobileUser$num_actions), 
                   'avg_sttp10' = mean(mobileUser$sttp10), 
                   'sttp10_per_action' = sum(mobileUser$sttp10)/ sum(mobileUser$num_actions),
                   'avg_num_search' = mean(mobileUser$num_search), 
                   'avg_num_filter' = mean(mobileUser$num_filter))


Android_App = c('num_session'= nrow(androidUser), 
            'distinct_users' = nrow(androidUser), 
            'avg_browsing_time' = mean(androidUser$browsing_time),
            'browsing_time_per_action' = sum(androidUser$browsing_time)/ sum(androidUser$num_actions),
            'avg_num_actions' = mean(androidUser$num_actions), 
            'avg_sttp10' = mean(androidUser$sttp10),  
            'sttp10_per_action' = sum(androidUser$sttp10)/ sum(androidUser$num_actions),
            'avg_num_search' = "N/A", 
            'avg_num_filter' = "N/A")

IOS_App = c('num_session'= nrow(iosUser), 
            'distinct_users' = nrow(iosUser), 
            'avg_browsing_time' = mean(iosUser$browsing_time),
            'browsing_time_per_action' = sum(iosUser$browsing_time)/ sum(iosUser$num_actions),
            'avg_num_actions' = mean(iosUser$num_actions), 
            'avg_sttp10' = mean(iosUser$sttp10), 
            'sttp10_per_action' = sum(iosUser$sttp10)/ sum(iosUser$num_actions),
            'avg_num_search' = "N/A", 
            'avg_num_filter' = "N/A")

platformResult = rbind(pc, Mobile_NoneApp, Android_App, IOS_App)

