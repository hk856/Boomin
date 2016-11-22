library(dplyr)
library(reshape)
library(ggplot2)
library(reshape2)
library(stats4)
library(splines)
library(VGAM)

setwd("/Users/ZW/Desktop/Courses/MPS Project/Data")

#read csv
theDataset <- read.csv(file = "data1.csv", header = TRUE, sep = "," )

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


#filter users. Only keep those have uuid and ua
filteredUsers <- filter(theDataset, uuid != "", ua != "")
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
                  sttp = filteredUsers$sttp, ts = filteredUsers$ts,
                  platform = filteredUsers$ua, filtered = filteredUsers$filtered, 
                  radio = filteredUsers$radio, checkBox = filteredUsers$checkBox, 
                  priceRange = filteredUsers$priceRange, orderBy = filteredUsers$orderBy, 
                  custom_range = filteredUsers$customRange, search = filteredUsers$search)
df = as.data.frame(valuePair)
#cast the dataframe
m <- matrix(0, ncol = 12, nrow = length(freqDF[,1]))
output = data.frame(m)

colnames(output) <- c("uuid","num_actions","sttp10","browsing_time","platform",
                      "num_filter","num_radio_f","num_checkBox_f",
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

write.csv(output, file = "functionAggregation.csv")

fa <- read.csv(file = "functionAggregation.csv", header = TRUE, sep = "," )

#Plots start here

qplot(browsing_time, data = output, geom = "histogram", binwidth = 100,
      xlim = c(0,2000), ylim = c(0,20000), col="red")

ggplot(data=output, aes(num_filter)) + 
  geom_histogram(breaks=seq(1, 10, by =1), 
                 col="red", 
                 aes(fill=..count..))

ggplot(data=output, aes(num_actions)) + 
  geom_histogram(breaks=seq(1, 100, by =1), 
                 col="red", 
                 aes(fill=..count..))

# clustering and its plots
set.seed(20)
faCluster <- kmeans(fa[, 3,5], 3, nstart = 20)
faCluster

faCluster$cluster <- as.factor(faCluster$cluster)
ggplot(fa, aes(num_actions, browsing_time, color = sttp10)) + geom_point()


fa()

fa$X <- NULL
fa$uuid <- NULL
fa$platform <- NULL
fa$num_radio_f <- NULL
fa$num_checkBox_f <- NULL
fa$num_priceRange_f <- NULL
fa$num_orderBy_f <- NULL
fa$num_custom_range <- NULL

clu<-kmeans(fa, 2)
clu

fit <- lm(fa$sttp10 ~ fa$browsing_time + fa$num_actions + fa$num_filter + fa$num_search)
summary(fit) #show results

clu$centers



clubarData <- clu$centers
c1<- c(6.83/(6.83+14.14),0.81/(0.81+1.59),1051.19/(1051.19+40705.79),0.038/(0.038+0.076),0.007/(0.007+0.015))
c2<-c(14.14/(6.83+14.14),1.59/(0.81+1.59),40705.79/(1051.19+40705.79),0.076/(0.038+0.076),0.015/(0.007+0.015))
clulbls <- c("num_actions","sttp10","browsing_time","num_filter","num_search")
clulbls
clubar <- data.frame(clulbls,c1,c2)

q <- plot_ly(clubar, x = ~clulbls, y = ~c1, type = 'bar', name = 'Cluster 1', marker = list(color = 'rgb(102,178,255)')) %>%
  add_trace(y = ~c2, name = 'Cluster 2', marker = list(color = 'rgb(255,153,255)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')
q

