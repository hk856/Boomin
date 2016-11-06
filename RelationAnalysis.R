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