

### read train and test set data
train = read.csv("train.csv")
test = read.csv("test.csv")

### checking how many rows have missing data
sum(!complete.cases(train))
sum(!complete.cases(test))

# train set have 684 rows and test set 315 rows with incomplete data
# about 15% of train and test set have missing data




### cheching doest some variable (colum) have large number of missing values
train.sapply = sapply(train, function(x) sum(is.na(x)))
test.sapply = sapply(test, function(x) sum(is.na(x)))
table(train.sapply)
table(test.sapply)

# only variable that have missing values is YOB (Year of Born)
# cheching summary statistic for YOB

summary(train$YOB)
summary(test$YOB)

# years of born in train set range from 1900 to 2039 and in test set from
# 1881 to 2039, clearly this isn't correct

# create copy of Happy variable and remove it from train set
Happy <- train$Happy
train$Happy <- NULL

# join train and test set data in one dataframe
data = rbind(train, test)

# remove UserID from dataset since UserID cannot predict level of happines
# since it's random assigned variable

data$UserID <- NULL

# (2014 - YOB) since competition was in 2014 (assumption)
data$YOB = 2014 - data$YOB

# clear outlier's age by NA
data$YOB[data$YOB < 14 | data$YOB > 100] <- NA

# oldest age now is 83 and not 133 which is much more realistic scenario
# missing variables rise from 998 to 1013

# replace YOB with YOBc 

set.seed(88)
library(mice)
imputed_data = complete(mice( data ))
summary(imputed_data$YOB)

# split dataset into cleaned train and test set
# and join Happy variable to train set

train.im = cbind(Happy, head(imputed_data, nrow(train)))
test.im = tail(imputed_data, nrow(test))

# Linear regression model
lmModel = lm(Happy~ ., data=train.im)
predLm.train = predict(lmModel, data=train.im)
table(train.im$Happy, predLm.train > 0.5)

#accuracy on train set
(1236+2085)/nrow(train.im)
# 0.7189868

# Logistic regression model
set.seed(88)
LogModel = glm(Happy~ ., data=train.im, family="binomial")
predLog.train = predict(LogModel, data=train.im)
summary(predLog.train)

table(train.c$Happy, predLog.train > 0.5)

# model accuracy on train set
(1591+1661)/nrow(train.im)
# accuracy is 0.7040485

## CART tree

library(rpart)
library(rpart.plot)

CART = rpart(Happy~ ., data=train.im, method="class", control=rpart.control(minsplit=30, cp=0.005))
predCART.train = predict(CART, data=train.im)[,2]

table(train.im$Happy, predCART.train > 0.5)

# model accuracy on train set
(2167+897)/nrow(train.im)
# 0.663347

submission = data.frame(UserID = test$UserID, Probability1 = predictTest)
write.csv(submission, "submission.csv", row.names=FALSE)

#################### Infulential questions ###################

rpart.plot(CART)

# Q118237 - Do you feel like you are "in over-your-head" in any aspect of your life right now?

# -> NO
### Q101162 - Are you generally more of an optimist or a pessimist?
### Q107869 - Do you feel like you're "normal"?
### Q98197 - Do you pray or meditate on a regular basis?

# -> YES
### Q121011 - Changing or losing a job, getting married or divorced, the death of a close relative, moving,
### a major health issue, bankruptcy...all are life events that can create high stress for people.
### Have you experienced any of these in 2013?
### Q102289 - Does your life feel adventurous?
### Q108617 - Do you live in a single-parent household?