library(dplyr)
library(reshape)
library(ggplot2)
library(reshape2)
library(plotly)
theDataset <- read.csv(file = "data1.csv", header = TRUE, sep = "," )

only10 <- filter(theDataset, sttp==10)

cidCount10<-only10 %>%
  group_by(cid) %>%
  summarise(n = n())

cidCount<-theDataset %>%
  group_by(cid) %>%
  summarise(n = n())

cidSorted <- arrange(cidCount, desc(n),cid)

cidTop10 <- head(cidSorted,11)

cidSorted10 <- arrange(cidCount10, desc(n),cid)

cidTop1010 <- head(cidSorted10,10)

cidTop10
cidTop1010

lblsNo10 <- c("Mobile Phone","Sneakers","A/C","Phone Case","Electronic Fan","TV","Tablets","Laptops","Watch","Sunglasses")
lbls10 <- c("Mobile Phone","Phone Case","Sneakers","A/C","Electronic Fan","Watch","TV","Tablets","Swimming Pants","Sunglasses")
cidTop10PieNo10 <- c(173435,46277,26925,24086,20899,18824,18669,17964,15924,15423)
cidTop1010Pie <- c(14722,5141,3361,2527,1839,1491,1448,1412,1367,1362)
cidTop10Pie <- c(173435,24086,46277,26925,20899,15924,18824,18669,11096,15423)
lbls <- c("Mobile Phone","Phone Case","Sneakers","A/C","Electronic Fan","Watch","TV","Tablets","Swimming Pants","Sunglasses")
cidTop1010Pie <- select(cidTop1010, n)
cidTop1010Pie
piepercent<- round(100*cidTop10Pie/sum(cidTop10Pie), 1)

barData <- data.frame(lbls,cidTop10Pie,lbls10,cidTop1010Pie)

barData

p <- plot_ly(barData, x = ~lbls, y = ~cidTop10Pie, type = 'bar', name = 'Top10', marker = list(color = 'rgb(102,178,255)')) %>%
  add_trace(y = ~cidTop1010Pie, name = 'Top1010', marker = list(color = 'rgb(255,153,255)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')
p


# Plot the chart.
#pie(cidTop10Pie, labels = piepercent, main = "Top 10 Categories Chart",col = rainbow(length(cidTop10Pie)))
#legend("topright",c("Mobile Phone","Sneakers","A/C","Phone Case","Electronic Fan","TV","Tablets","Laptops","Watch","Sunglasses"), fill = rainbow(length(cidTop10Pie)))

