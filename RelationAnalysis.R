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

mydata = cbind(num_action = aggregatedDataset$num_actions,
                  browsing_time = aggregatedDataset$browsing_time,
                  sttp10 = aggregatedDataset$sttp10,
                  num_filter = aggregatedDataset$num_filter,
                  num_search = aggregatedDataset$num_search)

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(mydata,2)
aggregatedDataset <- data.frame(aggregatedDataset, fit$cluster)
head(aggregatedDataset)
fit$centers

#fit <- lm(aggregatedDataset$sttp10 ~ aggregatedDataset$browsing_time + aggregatedDataset$num_actions + aggregatedDataset$num_filter + aggregatedDataset$num_search)
#summary(fit) #show results

length(aggregatedDataset[,1])

#yoochoose competition.

mean(aggregatedDataset$num_actions)
mean(aggregatedDataset$num_search)
mean(aggregatedDataset$num_filter)
mean(aggregatedDataset$sttp10)
mean(aggregatedDataset$browsing_time)