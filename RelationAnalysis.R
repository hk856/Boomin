setwd("/Users/Heng/Documents/CornellWork/MPS_Project/GIT")

#read csv
aggregatedDataset <- read.csv(file = "functionAggregation.csv", header = TRUE, sep = "," )

aggregatedDataset$X <- NULL
aggregatedDataset$uuid <- NULL
aggregatedDataset$platform <- NULL
aggregatedDataset$num_radio_f <- NULL
aggregatedDataset$num_checkBox_f <- NULL
aggregatedDataset$num_priceRange_f <- NULL
aggregatedDataset$num_orderBy_f <- NULL
aggregatedDataset$num_custom_range <- NULL



mydata = cbind(num_action = aggregatedDataset$total_num_actions,
               browsing_time = aggregatedDataset$avg_browsing_time,
               num_visit = aggregatedDataset$total_visits,
               sttp10 = aggregatedDataset$total_sttp10,
               num_filter = aggregatedDataset$num_filter,
               num_search = aggregatedDataset$num_search)


# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


mydata[!is.finite(mydata)] <- 0

fit <- kmeans(mydata,2)
aggregatedDataset <- data.frame(aggregatedDataset, fit$cluster)
head(aggregatedDataset)
fit$centers




fit <- lm(aggregatedDataset$sttp10 ~ aggregatedDataset$browsing_time + aggregatedDataset$num_actions + aggregatedDataset$num_filter + aggregatedDataset$num_search)
summary(fit) #show results

#fit <- lm(aggregatedDataset$sttp10 ~ aggregatedDataset$browsing_time + aggregatedDataset$num_actions + aggregatedDataset$num_filter + aggregatedDataset$num_search)
#summary(fit) #show results

length(aggregatedDataset[,1])

aggregatedDataset$avg_browsing_time[!is.finite(aggregatedDataset$avg_browsing_time)] <- NA
#yoochoose competition.
names(aggregatedDataset)[names(aggregatedDataset) =='fit.cluster'] <- 'cluster' 
windowShopper <- filter(aggregatedDataset, aggregatedDataset$cluster=='1')
hasteUser <- filter(aggregatedDataset, aggregatedDataset$cluster=='2')

mean(aggregatedDataset$total_num_actions)
mean(aggregatedDataset$num_search)
mean(aggregatedDataset$num_filter)
mean(aggregatedDataset$total_sttp10)

aggregatedDataset$avg_browsing_time[!is.finite(aggregatedDataset$avg_browsing_time)] <- NA
mean(aggregatedDataset$avg_browsing_time, na.rm = TRUE)


# create table to reflect userage of top 10 categories
t40 <- filter(aggregatedDataset, aggregatedDataset$cid40 >0)
t379 <- filter(aggregatedDataset, aggregatedDataset$cid379>0)
t407 <- filter(aggregatedDataset, aggregatedDataset$cid407>0)
t579 <- filter(aggregatedDataset, aggregatedDataset$cid579>0)
t427 <- filter(aggregatedDataset, aggregatedDataset$cid427>0)
t12 <- filter(aggregatedDataset, aggregatedDataset$cid12>0)
t105 <- filter(aggregatedDataset, aggregatedDataset$cid1105>0)
t25 <- filter(aggregatedDataset, aggregatedDataset$cid25>0)
t334 <- filter(aggregatedDataset, aggregatedDataset$cid334>0)
t790 <- filter(aggregatedDataset, aggregatedDataset$cid790>0)



#Window Shopper
count <- nrow(windowShopper)
avg_browsing_time <- mean(windowShopper$avg_browsing_time, na.rm = TRUE)
avg_visits <- mean(windowShopper$total_visits)
avg_num_actions <- mean(windowShopper$total_num_actions) / mean(windowShopper$total_visits)
action_rate <-avg_browsing_time / avg_num_actions
avg_sttp10 <- mean(windowShopper$total_sttp10) / mean(windowShopper$total_visits)
avg_num_search <- mean(windowShopper$num_search)
avg_num_filter <- mean(windowShopper$num_filter)
WINDOW_SHOPPER <- c(count,avg_browsing_time,avg_visits,avg_num_actions,avg_sttp10,avg_num_search,avg_num_filter)

#Haste Shopper
count <- nrow(hasteUser)
avg_browsing_time <- mean(hasteUser$avg_browsing_time, na.rm = TRUE)
avg_visits <- mean(hasteUser$total_visits)
avg_num_actions <- mean(hasteUser$total_num_actions) / mean(hasteUser$total_visits)
action_rate <-avg_browsing_time / avg_num_actions
avg_sttp10 <- mean(hasteUser$total_sttp10) / mean(hasteUser$total_visits)
avg_num_search <- mean(hasteUser$num_search)
avg_num_filter <- mean(hasteUser$num_filter)
HASTE_SHOPPER <- c(count,avg_browsing_time,avg_visits,avg_num_actions,avg_sttp10,avg_num_search,avg_num_filter)



mobile_phone <- c('count'=nrow(t40), 'avg_browsing_time' = mean(t40$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t40$total_visits),'avg_num_actions'=mean(t40$total_num_actions), 'avg_sttp10'=mean(t40$total_sttp10), 'avg_num_search'=mean(t40$num_search), 'avg_num_filter'=mean(t40$num_filter))
sneaker <- c('count'=nrow(t379),'avg_browsing_time' = mean(t379$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t379$total_visits),'avg_num_actions'=mean(t379$total_num_actions), 'avg_sttp10'=mean(t379$total_sttp10), 'avg_num_search'=mean(t379$num_search), 'avg_num_filter'=mean(t379$num_filter))
AC <- c('count'=nrow(t407),'avg_browsing_time' = mean(t407$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t407$total_visits),'avg_num_actions'=mean(t407$total_num_actions), 'avg_sttp10'=mean(t407$total_sttp10), 'avg_num_search'=mean(t407$num_search), 'avg_num_filter'=mean(t407$num_filter))
phone_case <- c('count'=nrow(t579),'avg_browsing_time' = mean(t579$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t579$total_visits),'avg_num_actions'=mean(t579$total_num_actions), 'avg_sttp10'=mean(t579$total_sttp10), 'avg_num_search'=mean(t579$num_search), 'avg_num_filter'=mean(t579$num_filter))
electronic_fan <- c('count'=nrow(t427),'avg_browsing_time' = mean(t427$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t427$total_visits),'avg_num_actions'=mean(t427$total_num_actions), 'avg_sttp10'=mean(t427$total_sttp10), 'avg_num_search'=mean(t427$num_search), 'avg_num_filter'=mean(t427$num_filter))
TV <- c('count'=nrow(t12),'avg_browsing_time' = mean(t12$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t12$total_visits),'avg_num_actions'=mean(t12$total_num_actions), 'avg_sttp10'=mean(t12$total_sttp10), 'avg_num_search'=mean(t12$num_search), 'avg_num_filter'=mean(t12$num_filter))
multimedia <- c('count'=nrow(t105),'avg_browsing_time' = mean(t105$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t105$total_visits),'avg_num_actions'=mean(t105$total_num_actions), 'avg_sttp10'=mean(t105$total_sttp10), 'avg_num_search'=mean(t105$num_search), 'avg_num_filter'=mean(t105$num_filter))
laptop <- c('count'=nrow(t25),'avg_browsing_time' = mean(t25$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t25$total_visits),'avg_num_actions'=mean(t25$total_num_actions), 'avg_sttp10'=mean(t25$total_sttp10), 'avg_num_search'=mean(t25$num_search), 'avg_num_filter'=mean(t25$num_filter))
watch <- c('count'=nrow(t334),'avg_browsing_time' = mean(t334$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t334$total_visits),'avg_num_actions'=mean(t334$total_num_actions), 'avg_sttp10'=mean(t334$total_sttp10), 'avg_num_search'=mean(t334$num_search), 'avg_num_filter'=mean(t334$num_filter))
sunglass <- c('count'=nrow(t790),'avg_browsing_time' = mean(t790$avg_browsing_time, na.rm = TRUE),'avg_visits'=mean(t790$total_visits),'avg_num_actions'=mean(t790$total_num_actions), 'avg_sttp10'=mean(t790$total_sttp10), 'avg_num_search'=mean(t790$num_search), 'avg_num_filter'=mean(t790$num_filter))

top10result <- rbind(mobile_phone,sneaker,AC,phone_case,electronic_fan,TV,multimedia,laptop,watch,sunglass,WINDOW_SHOPPER,HASTE_SHOPPER)
test <- rbind(WINDOW_SHOPPER,HASTE_SHOPPER)
