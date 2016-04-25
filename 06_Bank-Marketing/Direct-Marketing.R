#1 - age (numeric)
#2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
#                       "blue-collar","self-employed","retired","technician","services") 
#3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
#4 - education (categorical: "unknown","secondary","primary","tertiary")
#5 - default: has credit in default? (binary: "yes","no")
#6 - balance: average yearly balance, in euros (numeric) 
#7 - housing: has housing loan? (binary: "yes","no")
#8 - loan: has personal loan? (binary: "yes","no")
# related with the last contact of the current campaign:
#9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
#10 - day: last contact day of the month (numeric)
#11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
#12 - duration: last contact duration, in seconds (numeric)
## other attributes:
#13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
#15 - previous: number of contacts performed before this campaign and for this client (numeric)
#16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")

#Output variable (desired target):
#  17 - y - has the client subscribed a term deposit? (binary: "yes","no")

library(rpart)
library(rpart.plot)
library(caret)
library(gbm)
library(ipred)
library(pROC)

bank <- read.csv("bank-full.csv", sep=";")

#AUC require numeric or ordered factor
bank$y = as.character(bank$y)
bank$y[bank$y=="no"] <- 0
bank$y[bank$y=="yes"] <- 1
bank$y = as.integer(bank$y)

## set the seed to make partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(bank)), size = floor(0.75 * nrow(bank)))

train <- bank[train_ind, ]
test <- bank[-train_ind, ]

#fast CART test
CART = rpart(y~., data=train, method="class")

#prediction on train set
preCARTtr=predict(CART, type="class")
table(train$y, preCARTtr)
(29168+1399)/nrow(train)
# 0.9014687

#prediction on test set
preCARTte=predict(CART, newdata=test, type="class")
preCARTte = as.integer(preCARTte)
table(test$y, preCARTte)
(9735+447)/nrow(test)
# 0.9008228

#calculate AUC
auc <- roc(test$y, preCARTte)
print(auc.cv$auc) 
# Area under the curve: 0.7451

#instead of splitting the data into 2 parts of train and test, 
#data is split into 3 parts: ensembleData, blenderData, and testingData:
  
set.seed(1234)
bank.cv <- bank[sample(nrow(bank)),]
split <- floor(nrow(bank.cv)/3)
ensembleData <- bank.cv[0:split,]
blenderData <- bank.cv[(split+1):(split*2),]
testingData <- bank.cv[(split*2+1):nrow(bank.cv),]

#We assign the outcome name to labelName and the predictor variables to predictors:
  
labelName <- 'y'
predictors <- names(ensembleData)[names(ensembleData) != labelName]

#We create a caret trainControl object to control the number of cross-validations performed (the more the better but for breivity we only require 3):

myControl <- trainControl(method='cv', number=3, returnResamp='none')

#We run the data on a gbm model without any enembling to use as a comparative benchmark:

modelCART <- train(blenderData[,predictors], blenderData[,labelName], method='rpart', trControl=myControl)
predCART <- predict(object=modelCART, testingData[,predictors])

#calculate AUC
auc.cv <- roc(testingData[,labelName], predCART)
print(auc.cv$auc) 
#Area under the curve: 0.7451

#Now use 3 models - gbm, rpart, and treebag as part of our ensembles of models and train them with the ensembleData data set:

model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='gbm', trControl=myControl)
model_rpart <- train(ensembleData[,predictors], ensembleData[,labelName], method='rpart', trControl=myControl)
model_treebag <- train(ensembleData[,predictors], ensembleData[,labelName], method='treebag', trControl=myControl)

#After our 3 models are trained, we use them to predict 6 cylinder vehicles on the other two data sets: blenderData and testingData - yes, both!! We need to do this to harvest the predictions from both data sets as we’re going to add those predictions as new features to the same data sets. So, as we have 3 models, we’re going to add three new columns to both blenderData and testingData:
  
blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$rf_PROB <- predict(object=model_rpart, blenderData[,predictors])
blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])

testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
testingData$rf_PROB <- predict(object=model_rpart, testingData[,predictors])
testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])


#Now we train a final blending model on the old data and the new predictions (we use gbm but that is completely arbitrary):
  
predictors <- names(blenderData)[names(blenderData) != labelName]
final_blender_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)

#And we call predict and roc/auc functions to see how our blended ensemble model fared:
  
preds <- predict(object=final_blender_model, testingData[,predictors])
auc_f <- roc(testingData[,labelName], preds)
print(auc_f$auc)
#Area under the curve: 0.922

table(testingData$y,preds>0.5)
(12852+747)/nrow(testingData)
#0.902329

### CONCLUSION: Ensembles model have AUC of 0.922 and accuracy 90,23%,
### while CV and non-CV AUC of 0.7451 and accuracy 90,14%, but
### Ensemble have superior classification performance.