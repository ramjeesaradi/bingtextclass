setwd("~/Documents/BingHackathon/HackCode/")
library(e1071)
require(DMwR)
toNumeric <- function (data) {
  for( i in names(data) )
  {
    if( i == "topic"){
      data[,i] <- as.ordered(data[,i])
    }
    else if(class(data[,i])=="integer")
    {
      data[,i]  <-as.numeric(data[,i] )
      
    }
  }
  return(data)
}
library(caret)
library(unbalanced)
library(Metrics)
library(caTools)
library(e1071)
library(randomForest)

dta <- read.csv("final_data_1.csv")
#dta <- final_data
impVar <- as.character(read.csv("impfeature.csv",header = FALSE)[,1])

train <- toNumeric(dta[dta$publication!=0,])
test <- toNumeric(dta[dta$publication==0,])

train_top <- train[,names(dta) %in% append(impVar,"topic")]

test_top <- test
#####################################
set.seed(93)
spl = sample.split(as.factor(train_top$topic), 0.7)
trclmn <- !(names(train_top) %in% "topic")
target <- as.integer(train_top$topic[spl])
target[target!=2]<- 0
target[target==2]<- 1
indp <- train_top[spl,]
blnc <- SMOTE(indp$topic ~ . ,indp[,c(trclmn,"topic")] ,k=5,perc.over = 1400,perc.under=140)

blnc <- as.data.frame(blnc)
names(blnc) <-  c(names(train_top)[!(names(train_top) %in% "topic")],"topic")
blnc$topic[blnc$topic==1]<-2
blnc <- blnc[blnc$topic==2,]
indp <-indp[indp$topic!=2,names(blnc)]

blnc1<-rbind(indp, blnc)

#######################################################RF####################

rf1 <-randomForest(train_top[spl,trclmn],as.factor(train_top$topic[spl]),train_top[!spl,trclmn],as.factor(train_top$topic[!spl]), ntree=200)
rf2 <-randomForest(blnc1[,trclmn],as.factor(blnc1$topic),train_top[!spl,trclmn],as.factor(train_top$topic[!spl]), ntree=200)

outdta <-  predict(rf,newdata = test)
###################################################
svmfit <- svm(x = train_top[spl,trclmn],y=train_top$topic[spl],
              type='one-classification',
              nu=0.5,
              scale=TRUE,
              kernel="radial")
svm.pred<-predict(svmfit,train_top[!spl,trclmn])
confusionMatrixTable<-table(Predicted=svm.pred,Reference = train_top[!spl,"topic"])
