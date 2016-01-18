library(caret)
setwd("~/Documents/BingHackathon/HackCode/")
trndta <- read.csv("BingHackathonTrainingData.txt",header = FALSE,sep = "\t")
names(trndta) <- c("ID","topic","pYear","author", "title","summary")
summary(trndta$topic)
authors <- sapply(trndta$author,function (x) strsplit(as.character(x),";" ))

getunique <- function (tlist ) {
  uniquelist <- c(NULL)
  for (i in 1:length(tlist)){
    uniquelist <- unique(append(uniquelist,tlist[[i]]))
  }
  return(uniquelist)
}

getunique(authors)
