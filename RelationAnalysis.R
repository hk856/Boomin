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

fit <- kmeans(aggregatedDataset,2)
aggregatedDataset <- data.frame(aggregatedDataset, fit$cluster)



fit <- lm(aggregatedDataset$sttp10 ~ aggregatedDataset$browsing_time + aggregatedDataset$num_actions + aggregatedDataset$num_filter + aggregatedDataset$num_search)
summary(fit) #show results

length(aggregatedDataset[,1])

#yoochoose competition.

mean(aggregatedDataset$num_actions)
mean(aggregatedDataset$num_search)
mean(aggregatedDataset$num_filter)
mean(aggregatedDataset$sttp10)
mean(aggregatedDataset$browsing_time)


# create table to reflect userage of top 10 categories
functionAggregation <- read.csv(file = "functionAggregation.csv", header = TRUE, sep = "," )

t40 <- filter(functionAggregation, functionAggregation$cid40 >0)
t379 <- filter(functionAggregation, functionAggregation$cid379>0)
t407 <- filter(functionAggregation, functionAggregation$cid407>0)
t579 <- filter(functionAggregation, functionAggregation$cid579>0)
t427 <- filter(functionAggregation, functionAggregation$cid427>0)
t12 <- filter(functionAggregation, functionAggregation$cid12>0)
t105 <- filter(functionAggregation, functionAggregation$cid1105>0)
t25 <- filter(functionAggregation, functionAggregation$cid25>0)
t334 <- filter(functionAggregation, functionAggregation$cid334>0)
t790 <- filter(functionAggregation, functionAggregation$cid790>0)

mobile_phone <- c('avg_browsing_time' = mean(t40$browsing_time),'avg_num_actions'=mean(t40$num_actions), 'avg_sttp10'=mean(t40$sttp10), 'avg_num_search'=mean(t40$num_search), 'avg_num_filter'=mean(t40$num_filter))
sneaker <- c('avg_browsing_time' = mean(t379$browsing_time),'avg_num_actions'=mean(t379$num_actions), 'avg_sttp10'=mean(t379$sttp10), 'avg_num_search'=mean(t379$num_search), 'avg_num_filter'=mean(t379$num_filter))
AC <- c('avg_browsing_time' = mean(t407$browsing_time),'avg_num_actions'=mean(t407$num_actions), 'avg_sttp10'=mean(t407$sttp10), 'avg_num_search'=mean(t407$num_search), 'avg_num_filter'=mean(t407$num_filter))
phone_case <- c('avg_browsing_time' = mean(t579$browsing_time),'avg_num_actions'=mean(t579$num_actions), 'avg_sttp10'=mean(t579$sttp10), 'avg_num_search'=mean(t579$num_search), 'avg_num_filter'=mean(t579$num_filter))
electronic_fan <- c('avg_browsing_time' = mean(t427$browsing_time),'avg_num_actions'=mean(t427$num_actions), 'avg_sttp10'=mean(t427$sttp10), 'avg_num_search'=mean(t427$num_search), 'avg_num_filter'=mean(t427$num_filter))
TV <- c('avg_browsing_time' = mean(t12$browsing_time),'avg_num_actions'=mean(t12$num_actions), 'avg_sttp10'=mean(t12$sttp10), 'avg_num_search'=mean(t12$num_search), 'avg_num_filter'=mean(t12$num_filter))
multimedia <- c('avg_browsing_time' = mean(t105$browsing_time),'avg_num_actions'=mean(t105$num_actions), 'avg_sttp10'=mean(t105$sttp10), 'avg_num_search'=mean(t105$num_search), 'avg_num_filter'=mean(t105$num_filter))
laptop <- c('avg_browsing_time' = mean(t25$browsing_time),'avg_num_actions'=mean(t25$num_actions), 'avg_sttp10'=mean(t25$sttp10), 'avg_num_search'=mean(t25$num_search), 'avg_num_filter'=mean(t25$num_filter))
watch <- c('avg_browsing_time' = mean(t334$browsing_time),'avg_num_actions'=mean(t334$num_actions), 'avg_sttp10'=mean(t334$sttp10), 'avg_num_search'=mean(t334$num_search), 'avg_num_filter'=mean(t334$num_filter))
sunglass <- c('avg_browsing_time' = mean(t790$browsing_time),'avg_num_actions'=mean(t790$num_actions), 'avg_sttp10'=mean(t790$sttp10), 'avg_num_search'=mean(t790$num_search), 'avg_num_filter'=mean(t790$num_filter))


top10result <- rbind(mobile_phone,sneaker,AC,phone_case,electronic_fan,TV,multimedia,laptop,watch,sunglass)


