# Examine the depth at which the users are willing to go to find products


onTheLeaf <- filter(theDataset,sttp == 11) # all browsing actions on a leaf 
allPages <- length(grep("page", theDataset$uri, perl=TRUE, value=FALSE) # all actions where page was displayed

page1 <- length(onTheLeaf[,1]) - allPages # the number of actions on the first page
returnPage1 <- length(grep("page=1\\b", theDataset$uri, perl=TRUE, value=FALSE))
page2 <- length(grep("page=2\\b", theDataset$uri, perl=TRUE, value=FALSE))
page3 <- length(grep("page=3\\b", theDataset$uri, perl=TRUE, value=FALSE))
page4 <- length(grep("page=4\\b", theDataset$uri, perl=TRUE, value=FALSE))
page5 <- length(grep("page=5\\b", theDataset$uri, perl=TRUE, value=FALSE))
page6 <- length(grep("page=6\\b", theDataset$uri, perl=TRUE, value=FALSE))
page7 <- length(grep("page=7\\b", theDataset$uri, perl=TRUE, value=FALSE))
page8 <- length(grep("page=8\\b", theDataset$uri, perl=TRUE, value=FALSE))
page9 <- length(grep("page=9\\b", theDataset$uri, perl=TRUE, value=FALSE))
page10 <- length(grep("page=10\\b", theDataset$uri, perl=TRUE, value=FALSE))       

#Sum all the actions in the 10 first pages and calculate the remaining page actions 

firstPages <- sum(returnPage1,page2,page3,page4,page5,page6,page7,page8,page9,page10)
backPages <- length(grep("page", theDataset$uri, perl=TRUE, value=FALSE)) - firstPages
)


# bind everything together using a vector 

attentionDistribution <- c(page1,page2,page3,page4,page5,page6,page7,page8,page9,page10,returnPage1,backPages)

# A simple bar plot summarizing how users allocate their attention

barplot(attentionDistribution, main = "attention distribution", ylab = "# of actions",xlab = "page",names.arg=c("1","2","3","4","5","6","7","8","9","10","1 ret","bp"))
