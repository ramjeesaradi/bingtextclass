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
library(textir)

corpus<- Corpus(VectorSource(com_data$authors))
corpus <- tm_map(corpus , PlainTextDocument)
corpus <- tm_map(corpus , removePunctuation)


dtm <- DocumentTermMatrix(corpus)
didf <- tfidf(dtm, normalize=TRUE)

spdtm <- removeSparseTerms(dtm , 0.50)
spdtm <- as.data.frame(as.matrix(spdtm))

colnames(spdtm)


colnames(spdtm) <- make.names(colnames(spdtm))

sort(colSums(spdtm) )
str(spdtm)
row.names(spdtm)<-NULL
author <- spdtm

colnames(author) <- paste("author", colnames(author), sep = "_")


############################################  data split for title ##################3

corpus<- Corpus(VectorSource(com_data$title))
corpus <- tm_map(corpus , PlainTextDocument)
corpus <- tm_map(corpus , removePunctuation)


dtm <- DocumentTermMatrix(corpus)
dtm

spdtm <- removeSparseTerms(dtm , 0.999)
spdtm <- as.data.frame(as.matrix(spdtm))

colnames(spdtm)


colnames(spdtm) <- make.names(colnames(spdtm))

sort(colSums(spdtm) )
str(spdtm)
row.names(spdtm)<-NULL
title <- spdtm
colSums(title)
colnames(title) <- paste("title", colnames(title), sep = "_")


############################################  data split for summary ##################3

corpus<- Corpus(VectorSource(com_data$summary))
corpus <- tm_map(corpus , PlainTextDocument)
corpus <- tm_map(corpus , removePunctuation)


dtm <- DocumentTermMatrix(corpus)
dtm

spdtm <- removeSparseTerms(dtm , 0.999)
spdtm <- as.data.frame(as.matrix(spdtm))

ncol(spdtm)


colnames(spdtm) <- make.names(colnames(spdtm))

sort(colSums(spdtm) )
str(spdtm)
row.names(spdtm)<-NULL
summary <- spdtm

sort(colSums(summary))
colnames(summary) <- paste("summary", colnames(summary), sep = "_")



####################   combination of data 
final_data<- cbind(com_data, title, author,summary)

write.csv(final_data , "final_data_1.csv")
############################################################

final_data$noOfWords<-NULL
for ( i in 1:nrow(final_data))
{
  final_data$noOfWords[i]<-(length(unique(data.frame(strsplit( gsub(".", " ",final_data$summary[i], fixed = TRUE), " "))[,1])))
}

final_data$noOflines<-NULL

for ( i in 1:nrow(final_data))
{
  final_data$noOflines[i]<-sum(data.frame(strsplit( gsub(".", " . ",final_data$summary[i], fixed = TRUE), " "))[,1]==".")
  
  
}



test_data_modified<-final_data[final_data$topic == 0 & final_data$publication == 0 , ]

train_data_modified<- final_data[final_data$publication != 0 , ]
str(train_data_modified)

table(train_data_modified[,2],train_data_modified[,3])
table(train_data_modified[,2])
aggregate(names(author)~. ,data = train_data_modified, sum)



library(caTools)

set.seed(144)

spl = sample.split(train_data_modified$topic, 0.999)

train = subset(train_data_modified, spl == TRUE , c(2 , 7:ncol(train_data_modified)) )
str(train)

test = subset(train_data_modified, spl == FALSE, c(2 , 7:ncol(train_data_modified)) )




library(randomForest)
numTrees<-1000
p<-NULL
i<-1
train1 <- train[,-1]
labels <- as.factor(train[, 1])

#log(labels+1)
rf <- randomForest(train1, labels, xtest=test[,-1], ntree=numTrees)
predictions <- data.frame(Id=test[ , 1] , Sales= rf$test$predicted )
rf
table(test[ , 1] , rf$test$predicted)

p<- rbind(p , predictions)
v
varImp(rf)
sort(importance(rf))



#####################33   clustering 


library(caret)

clust_data<-train_data_modified[,c( 7:ncol(train_data_modified))]
norm_clust_data<-train_data_modified[,c( 7:ncol(train_data_modified))]
str(clust_data)




#preproc = preProcess(clust_data)
#norm_clust_data = predict(preproc, clust_data)


colMeans(norm_clust_data)

distance<-dist(norm_clust_data, method = "euclidian")
h<- hclust(distance , method = "ward.D")
plot(h)
table(train_data_modified[,2] ,cutree(h , 3 ))





test <- tail(train1 , nrow(test))
train<- head( train1 , nrow(train))

################################################  decision tree 



library(rpart)
CARTb = rpart(topic ~ ., data=train, method="class")

p<- predict(CARTb , type = "class")


p<- predict(CARTb ,newdata <- test, type = "class")

table(train[,1],p)
m<-as.matrix(table(test[,1],p))
(m[1]+m[5]+m[9])/nrow(test)


  
  