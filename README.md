# titanic
titanic = read.csv("train.csv")
titanic.test = read.csv("test.csv")

library(readxl)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(ROSE)
library(randomForest)
library(Hmisc)
library(mice)
library(randomForest)
library(Boruta)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

titanic[,5] = as.numeric(titanic[,5])
titanic[,6] = as.numeric(titanic[,6])
sum(is.na(titanic))
simple = titanic[c("Age", "SibSp", "Sex")]
imputed = complete(mice(simple))

titanic$Age = imputed$Age
for(i in 1:ncol(titanic)){
  titanic[,i] = as.numeric(titanic[,i])
}
cor(titanic)
split = sample.split(titanic$Survived, SplitRatio = 0.75)
ptrain = subset(titanic, split == TRUE)
ptest = subset(titanic, split == FALSE)


#not in use
for(i in 1:ncol(titanic.test)){
  titanic.test[,i] = as.numeric(titanic.test[,i])
}
#

#CART accuracy=80.71%
tree = rpart(Survived ~ ., data = ptrain, method = "class" )
prp(tree)
predtrain = predict(tree, newdata = ptest, type = "class")
table(ptest$Survived, predtrain)

#Randomforest accuracy = 84.304%
#converting Survived variable to factor
ptrain$Survived = as.factor(ptrain$Survived)
ptest$Survived = as.factor(ptest$Survived)

titanic.forest= randomForest(Survived ~ ., data = ptrain)
pred.forest= predict(titanic.forest, newdata = ptest)
table(ptest$Survived, pred.forest)

#Boruta for checking less important variables. It is certainly not required
boruta.train = Boruta(Survived~ ., data = titanic)
print(boruta.train)
 
#by watching correlation b/w survived and other variables
#and deleting some of the columns. accuracy went low as earlier.
titanic$PassengerId= NULL
titanic$SibSp= NULL
# end

#XGBoost
dtrain= xgb.DMatrix(data=ptrain)
titanic.xgb = xgboost(data = ptrain,  eta = 0.3, max_depth = 6, nround=100,subsample= 1, colsample_bytree=1, eval_metric= "merror", objective="multi:softprob", num_class= 12, nthread= 3)




