
### KAGGLE COMPETITION - DEALING WITH THE TEXT DATA

#The dependent variable in this problem is the variable sold, which labels if an iPad listed on the eBay site was sold (equal to 1 if it did, and 0 if it did not). The dependent variable is provided in the training data set, but not the testing dataset. This is an important difference from what you are used to - you will not be able to see how well your model does on the test set until you make a submission on Kaggle. 

#The independent variables consist of 9 pieces of product data available at the time the iPad listing is posted, and a unique identifier:

#description = The text description of the product provided by the seller.
#biddable = Whether this is an auction (biddable=1) or a sale with a fixed price (biddable=0).
#startprice = The start price (in US Dollars) for the auction (if biddable=1) or the sale price (if biddable=0).
#condition = The condition of the product (new, used, etc.)
#cellular = Whether the iPad has cellular connectivity (cellular=1) or not (cellular=0).
#carrier = The cellular carrier for which the iPad is equipped (if cellular=1); listed as "None" if cellular=0.
#color = The color of the iPad.
#storage = The iPad's storage capacity (in gigabytes).
#productline = The name of the product being sold.

library(tm)
library(wordcloud)
library(SnowballC)
library(randomForest)
library(mice)
library(caret)
library(e1071)


#We are adding in the argument stringsAsFactors=FALSE, since we have some text fields
eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

#Then create a corpus from the description variable. You can use other variables in the dataset for text analytics, but we will just show you how to use this particular variable. 

CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)


#Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 

dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.99)
DescriptionWords = as.data.frame(as.matrix(sparse))

# vizualize wordcloud
wordcloud(colnames(DescriptionWords), colSums(DescriptionWords), scale=c(2, 0.5), ordered.colors=TRUE)

# Let's make sure our variable names are okay for R:

colnames(DescriptionWords) = make.names(colnames(DescriptionWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of DescriptionWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(eBayTrain) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTrain"

DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))

# The tail function takes the last "n" rows of DescriptionWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(eBayTest) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTest"

DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))

# Note that this split of DescriptionWords works to properly put the observations back into the training and testing sets, because of how we combined them together when we first made our corpus.

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!

DescriptionWordsTrain$sold = eBayTrain$sold

DescriptionWordsTrain$WordCount = eBayTrain$WordCount
DescriptionWordsTest$WordCount = eBayTest$WordCount

# Remember that you can always look at the structure of these data frames to understand what we have created

#Now let's create a logistic regression model using all of the variables:

DescriptionWordsLog = glm(sold ~ ., data=DescriptionWordsTrain, family=binomial)
summary(DescriptionWordsLog)
PredTRAINLog = predict(DescriptionWordsLog, data=DescriptionWordsTrain)
table(DescriptionWordsTrain$sold, PredTRAINLog>0.5)
(84+964)/nrow(DescriptionWordsTrain)
0.5631381

# And make predictions on our test set:

PredTest = predict(DescriptionWordsLog, newdata=DescriptionWordsTest, type="response")

# Now we can prepare our submission file for Kaggle:

MySubmissionLog = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmissionLog, "SubmissionDescriptionLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionDescriptionLog.csv" on the Kaggle website to use this as a submission to the competition


### Project Submission on Kaggle


# merge eBayTrain and eBayTest in one dataframe - eBay
eBay = rbind(eBayTrain[,2:9], eBayTest[,2:9])

# rename variable "condition"" = The condition of the product (new, used, etc.)
# into "state"" because word "condition" appear in corpus
# Template: names(df)[names(df) == 'old.var.name'] <- 'new.var.name'

names(eBay)[names(eBay) == 'condition'] <- 'state'

# impute missing variables
set.seed(88)
imputed_data = complete(mice( eBay ))

#split imputed eBay dataframe to train and test set
eBayTrainNEW = head(imputed_data, nrow(eBayTrain))
eBayTrainNEW$sold = eBayTrain$sold
eBayTestNEW = tail(imputed_data, nrow(eBayTest))

#Joining all independent variables together
train = cbind(DescriptionWordsTrain, eBayTrainNEW)
test= cbind(DescriptionWordsTest, eBayTestNEW)

str(eBayTrain[,2:9])
train[,82] <- NULL

# Cross-validation
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

train(sold ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )

#Random forest for cp=0.004
modelRF = randomForest(sold ~ ., data=train, method="class", cp=0.004)
predRF = predict(modelRF, data=train, type="class")
table(train$sold, predRF>0.5)
(885+624)/nrow(train)
# 0.8108544

PredTESTRF1 = predict(modelRF, newdata=test)
SubmissionRF = data.frame(UniqueID = eBayTest$UniqueID, Probability1 = PredTESTRF1)
write.csv(SubmissionRF, "SubmissionRF.csv", row.names=FALSE)

#celluar and condition are least important
varImpPlot(modelRF)



