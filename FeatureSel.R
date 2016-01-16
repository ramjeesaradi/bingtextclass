setwd("~/Documents/BingHackathon/HackCode/")

library(caret)
library(mlbench)
library(randomForest)
bngtxt <- read.csv("final_data_1.csv")
bngtxt <- bngtxt[bngtxt$publication != 0,]
drops <- c("X","summary","authors","title", "record")
bngtxt1 <- bngtxt[,!(names(bngtxt) %in% append(drops,"publication"))]
bngtxt2 <- bngtxt[,!(names(bngtxt) %in% append(drops,"topic"))]

control <- rfeControl(functions=rfFuncs, method="cv", number=2)
# run the RFE algorithm
results <- rfe(x = bngtxt1[,!(names(bngtxt1) %in% "topic")],y = as.string(bngtxt1[,"topic"]),sizes = c(100), rfeControl=control)

write.csv(results$variables,"variablescore.csv")
nrow(bngtxt)
  