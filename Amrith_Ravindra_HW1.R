############ CODE FOR DECISION TREE USING TREE PACKAGE ######################

install.packages("tree")
library(tree)
Bank = read.csv2("bank-full.csv")
temp = Bank
head(Bank)
setSize <- floor(0.67 * nrow(Bank))
set.seed(123) #set a seed for being able to replicate
rowIndices <- sample(seq_len(nrow(Bank)), size = setSize)
trainBank <- Bank[rowIndices, ]
testBank <- Bank[-rowIndices, ]
dtreeModel = tree(y ~., data = trainBank, split = c("gini"))
summary(dtreeModel)
names(dtreeModel)
dtreeModel$y
plot(dtreeModel)
text(dtreeModel, pos=3, cex=0.7, col = 'blue')

############## CODE FOR DECISION TREE USING RPART PACKAGE ###################

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
Bank = read.csv2("bank-full.csv")
head(Bank)
setSize <- floor(0.67 * nrow(Bank))
set.seed(123) #set a seed for being able to replicate
rowIndices <- sample(seq_len(nrow(Bank)), size = setSize)
trainBank <- Bank[rowIndices, ]
testBank <- Bank[-rowIndices, ]
dtreeModel2 = rpart(y ~., data = trainBank, method = 'class', parms = list(split="gini"))
summary(dtreeModel2)
names(dtreeModel2)
dtreeModel2$variable.importance
rpart.plot(dtreeModel2,extra=1, varlen=0)










###################### CODE FOR RANDOM FOREST ############################


install.packages("randomForest")
library(randomForest)
Bank = read.csv2("bank-full.csv")
rfModel = randomForest(formula = y~., data = Bank, ntree = 250, importance = TRUE, replace=TRUE)
summary(rfModel)
names(rfModel)
rfModel$confusion


#########################################################################
setSize <- floor(0.67 * nrow(Bank))
set.seed(123) #set a seed for being able to replicate
rowIndices <- sample(seq_len(nrow(Bank)), size = setSize)
trainBank <- Bank[rowIndices, ]
testBank <- Bank[-rowIndices, ]

rfModel2 = randomForest(formula = y~., data = trainBank, ntree = 500, mtry = 2, importance = TRUE, replace=TRUE, proximity=TRUE, sampsize=c(500,400))
rfModel2pred <- predict(object = rfModel2, newdata = testBank[,-4])
table(observed = testBank$y, predicted = rfModel2pred)
rfModel2$confusion
rfModel2
par(mfrow=c(1,2))
varImpPlot(rfModel2,main='Variable Importance Plot: Final Model',pch=16,col='blue')

#########################################################################
