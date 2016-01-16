## data loading
setwd("~/Documents/BingHackathon/HackCode/")

train_data<-read.csv("BingHackathonTrainingData.txt",sep='\t',header=F,stringsAsFactors = F)
test_data<-read.csv("BingHackathonTestData.txt",sep='\t',header=F,stringsAsFactors = F)

head(train_data)

# headers

names(train_data)<-c("record","topic","publication","authors","title","summary")
names(test_data)<-c("record","topic","publication","authors","title","summary")

## to convert the authors field and use the tm

train_data$authors<-gsub(" ", "",train_data$authors, fixed = TRUE)
train_data$authors<-gsub(";"," ",train_data$authors, fixed = TRUE)

test_data$authors<-gsub(" ", "",test_data$authors, fixed = TRUE)
test_data$authors<-gsub(";"," ",test_data$authors, fixed = TRUE)

train_data <- train_data[,colSums(is.na(train_data))<nrow(train_data)]

com_data<-rbind(train_data,test_data)
str(com_data)

############################################  data split for author ##################3
library(tm)

corpus<- Corpus(VectorSource(com_data$authors))
corpus <- tm_map(corpus , PlainTextDocument)
corpus <- tm_map(corpus , removePunctuation)


dtm <- DocumentTermMatrix(corpus)
dtm

spdtm <- removeSparseTerms(dtm , .997)
spdtm <- as.data.frame(as.matrix(spdtm))

colnames(spdtm)


colnames(spdtm) <- make.names(colnames(spdtm))

sort(colSums(spdtm) )
str(spdtm)
row.names(spdtm)<-NULL
author <- spdtm



############################################  data split for title ##################3

corpus<- Corpus(VectorSource(com_data$title))
corpus <- tm_map(corpus , PlainTextDocument)
corpus <- tm_map(corpus , removePunctuation)


dtm <- DocumentTermMatrix(corpus)
dtm

spdtm <- removeSparseTerms(dtm , .99)
spdtm <- as.data.frame(as.matrix(spdtm))

colnames(spdtm)


colnames(spdtm) <- make.names(colnames(spdtm))

sort(colSums(spdtm) )
str(spdtm)
row.names(spdtm)<-NULL
title <- spdtm


############################################  data split for summary ##################3

corpus<- Corpus(VectorSource(com_data$summary))
corpus <- tm_map(corpus , PlainTextDocument)
corpus <- tm_map(corpus , removePunctuation)


dtm <- DocumentTermMatrix(corpus)
dtm

spdtm <- removeSparseTerms(dtm , .96)
spdtm <- as.data.frame(as.matrix(spdtm))

ncol(spdtm)


colnames(spdtm) <- make.names(colnames(spdtm))

sort(colSums(spdtm) )
str(spdtm)
row.names(spdtm)<-NULL
summary <- spdtm
