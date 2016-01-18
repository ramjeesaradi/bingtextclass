setwd("~/Documents/BingHackathon/HackCode/")

library(caret)
library(mRMRe)
set.thread.count(200)
bngtxt <- final_data <- read.csv("final_data_1.csv")
bngtxt <- bngtxt[bngtxt$publication != 0,]
drops <- c("X","summary","authors","title", "record")
bngtxt1 <- bngtxt[,!(names(bngtxt) %in% append(drops,"publication"))]
write.csv(bngtxt1,"train_weka.csv", row.names = FALSE)
bngtxt2 <- bngtxt[,!(names(bngtxt) %in% append(drops,"topic"))]



#########################################convert all cols to numeric####################
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

#########################################run mRMRe###################################
rmrdta <- mRMR.data(toNumeric(bngtxt1))
corvar = mRMR.classic(data = rmrdta,target_indices = as.integer(which(names(bngtxt1)=="topic")),feature_count=1000)
impVar <- as.vector(solutions(corvar)[[1]])
write.csv(names(bngtxt1)[impVar],"impfeature.csv",row.names = FALSE)
class(corvar)
########################################################################################
rmrdta2 <- mRMR.data(toNumeric(bngtxt2))
corvar2 = mRMR.classic(data = rmrdta2,target_indices = as.integer(which(names(bngtxt2)=="publication")),feature_count=600)
impVar2 <- as.vector(solutions(corvar2)[[1]])
write.csv(names(bngtxt2)[impVar2],"impfeature2.csv",row.names = FALSE)

# run the RFE algorithm
control <- rfeControl(functions=rfFuncs, method="cv", number=2)
results <- rfe(x = bngtxt1[,!(names(bngtxt1) %in% "topic")],y = as.string(bngtxt1[,"topic"]),sizes = c(100), rfeControl=control)

write.csv(results$variables,"variablescore.csv")