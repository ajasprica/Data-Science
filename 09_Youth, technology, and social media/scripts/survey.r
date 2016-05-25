#read data from csv
# na.strings = c("", "NA") - pass NA to empty cells
surveytmp <- read.csv("surveytmp.csv", header = TRUE, na.strings = c("", " ","NA"), sep=";")

#dataframe dimension 
dim(surveytmp) 
#569 126 (original: 580 1599)

#missing values for each variable
missinValue <- sort(sapply(surveytmp, function(x) sum(is.na(x))))
missinValue

#remove score variable
surveytmp$score <- NULL

#gender variable as.factor
surveytmp$gender <- as.factor(surveytmp$gender)
levels(surveytmp$gender) <- c('Female','Male') 

#ordered factor
surveytmp$age <- ordered(surveytmp$age, levels = c("16-19", "20-24", "25-30", ">30"))
surveytmp$education <- ordered(surveytmp$education, levels = c("None", "Primary", "Secondary", "Certificate", "University", "Mas/PhD"))
surveytmp$occupation <- ordered(surveytmp$occupation, levels = c("Unemployed", "Casual", "Workforce", "Self-employed", "S-owner", "M-owner", "L-owner"))

#ordered factor for multiple collums
require(plyr)
surveytmp[,12:49] <- catcolwise( function(v) ordered(v, levels = c("Never", "Less than once a month", "Once a month", "Once every few weeks", "Once a week", "Every few days", "Once a day", "Multiple times day")))(surveytmp[,12:49])
surveytmp[,50:68] <- catcolwise( function(v) ordered(v, levels = c("Never", "Less than once a month", "Once a month", "Once every few weeks", "Once a week", "Every few days", "Once a day", "Always")))(surveytmp[,50:68])
surveytmp[,69:75] <- catcolwise( function(v) ordered(v, levels = c("Never", "Less than once a month", "Once a month", "Once every few weeks", "Once a week", "Every few days", "Once a day", "Multiple times day")))(surveytmp[,69:75])
surveytmp[,76:125] <- catcolwise( function(v) ordered(v, levels = c("Never", "Not very often", "Somewhat often", "Often", "Very often")))(surveytmp[,76:125])

#imputation
require(missForest)
im.miss <- missForest(surveytmp, maxiter = 10, ntree = 300)
#extract imputed dataset
survey <- im.miss$ximp

#save survey as *.csv
write.table(survey, file = "survey.csv", sep = ",")


#CART tree
library(rpart)
library(rpart.plot)

set.seed(88)
CART = rpart(gender ~ ., data=survey, method="class")
predCART = predict(CART, type="class")

#accuracy on train set
table(survey$gender, predCART)
(209+216)/nrow(survey)
#0.7469244

#plot tree
prp(CART)

#check for important variables
require(caret)
df <- varImp(CART, scale=FALSE) 
df <- subset(df, Overall > 0)
df


CART2 = rpart(gender ~ ., data=survey, method="class", cp
prp(CART2)

predCART2 = predict(CART2, type="class")
                    
table(survey$occupation, predCART2)
(202+17+161+29)/nrow(survey)
#0.7188049

