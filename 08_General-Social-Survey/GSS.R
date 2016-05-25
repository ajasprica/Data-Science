###### DATA ######

#### Codebook: 
#### https://d396qusza40orc.cloudfront.net/statistics%2Fproject%2Fgss1.html


# load dataset from web
load(url("http://bit.ly/dasi_gss_data"))

#save data as *.CSV
write.table(gss, file = "gss.csv", sep = ",")

gss <- read.csv("gss.csv")

dim(gss)
# Dimension of dataframe: 57061   114

####################################################

# Research question: What are most important factors 
# that infulence monthly income?

#####################################################

# coinc missing values count
sum(!complete.cases(gss$coninc))
# 5829

#missing values for full dataset
missinValue <- sort(sapply(gss, function(x) sum(is.na(x))))
missinValue
# some variables have extreme large number of missing values

gss_clean <- gss[!is.na(gss$coninc),]


library(rpart)
library(rpart.plot)

set.seed(88)
CART = rpart(coninc~ ., data=subset(gss_clean, select = - c(caseid, finrela)), control=rpart.control(minsplit=20, cp=0.001))

# use caret package to find important variables
require(caret)
df <- varImp(CART, scale=FALSE) 

#create subset of data and extract names and score
dfnew <- subset(df, Overall > 0.1)
names <- row.names(dfnew)
score <- dfnew$Overall

# variable importance dataframe
importance <- data.frame(names, score)
importance$names <- factor(importance$names, levels = importance[order(importance$score), "names"])

### EXIBIT 1 ###
require(ggplot2)
ggplot(importance, aes(x=names, y=score)) + xlab("Name") + ylab("Score") +
  geom_bar(stat="identity") + 
  coord_flip() + ggtitle("Variable Importance")

###### EXPLORATORY ######

### EXIBIT 2 ###
d <- ggplot(gss_clean,aes(x = age, y = coninc, colour=satfin)) + xlab("Age") + ylab("Income") + geom_point(size=2.5) + 
  theme(text = element_text(size=14),legend.position="top", 
        axis.text.x = element_text(angle=90, vjust=1))
d + scale_fill_manual(values=c("#F8766D", "#00BA38", "#0000ff"))

# run dev.off() if graph fail to plot

### EXIBIT 3 ###

p <- ggplot(gss_clean,aes(x = year, y = coninc, colour=satfin)) + xlab("Year") + ylab("Income") + geom_point(size=2.5) + 
  theme(text = element_text(size=14),legend.position="top", 
        axis.text.x = element_text(angle=90, vjust=1))
p + scale_fill_manual(values=c("#F8766D", "#00BA38", "#0000ff"))

### EXIBIT 4 ###

ggplot(aes(y=coninc,x=marital, fill=sex), data=gss_clean) + geom_boxplot() + xlab("Marital status") + ylab("Income")

table(gss_clean$sex)
#Female   Male 
#28189  23043

#t-test
t.test(gss_clean$coninc ~ gss_clean$sex)

1-41020.22/48763.65
#male have on average 15,9% higher salary

##### MODELING #########

gssNEW <- gss_clean[c("coninc","age","class", "degree", "educ", "finalter", "maeduc", "marital", 
                        "race", "satfin", "sei", "sex", "speduc", "spwrksta", "wrkstat",  "year",
                        "union","rifle","region","paeduc","owngun","suicide1","unemp","jobfind")]

missinValueNEW <- sort(sapply(gssNEW, function(x) sum(is.na(x))))
missinValueNEW


# define function to allow missing values per row
delete.na <- function(df, n=0) {
  log <- apply(df, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) <= n)
  df[logindex, ]
}

# allow maximum 5 NA's per observation, remove others
gssNEW = delete.na(gssNEW, 5)
57061-55882
#1179 observations removed

library(mice)
set.seed(88)
impute = complete(mice( gssNEW ))

write.table(impute, file = "imp.csv", sep = ",")

modelLM = lm(coninc~., data=subset(impute, select = - c(educ)))
summary(modelLM)

##############################################################################
# remove variables that have 60% or more missing values in variable (40,000+)
gss$uscitzn <- NULL
gss$getaid <- NULL
gss$rank <- NULL
gss$income06 <- NULL
gss$govaid <- NULL
gss$kidssol <- NULL
gss$parsol <- NULL
gss$caseid <- NULL

# irrelevant variables
gss$finrela <- NULL #personal opinion
gss$class <- NULL #personal opinion

# remove rows that have NA's in $coninc
gss_clean <- gss[!is.na(gss$coninc),]

# define function to allow missing values per row
delete.na <- function(df, n=0) {
  log <- apply(df, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) <= n)
  df[logindex, ]
}

# allow maximum 40 NA's per observation, remove others
gss_clean = delete.na(gss_clean, 40)
57061-38862
# 18199 observations removed

#random split to train and test
require(caTools)
set.seed(88) 
sample = sample.split(gss_clean$coninc, SplitRatio = 0.7)
train = subset(gss_clean, sample == TRUE)
test = subset(gss_clean, sample == FALSE)


# use CART tree since it handle missing data
library(rpart)
library(rpart.plot)

CART = rpart(coninc~ ., data=train, control=rpart.control(minsplit=20, cp=0.001))

predCART = predict(CART, newdata=test)

SSE = sum((test$coninc - predCART)^2)
SST = sum((test$coninc - mean(train$coninc))^2)
1 - SSE/SST
# 0.3848847 - made better prediction with deeper tree

rpart.plot(CART)
plot(test$coninc, predCART)

# variable importance
require(caret)
df <- data.frame(varImp(CART))

#age 1.67239889
#satfin 1.36097709
#speduc 1.06787497
#spwrksta 1.03845394
#educ 1.00606636
#sei 0.96654648
#wrkstat 0.78906663
#year 0.62700876
#marital 0.46219357
#degree 0.43598365
#finalter 0.41434833
#childs 0.25030310
#region 0.19672933
#sex 0.18458249
#maeduc 0.15735391
#owngun 0.14248936
#race 0.12997824
#incom16 0.12576455
#homosex 0.10410587
#premarsx 0.07266844
#paeduc 0.06394984
#relig 0.05546251
#partyid 0.05352884
#abdefect 0.05211461
#tvhours 0.05205494
#unemp 0.04827879
#rifle 0.04685929
#abnomore 0.03939061
#wrkslf 0.03650347
#conlegis 0.03459802
#union 0.02831657
#absingle 0.02482961
#granborn 0.02014007
#agekdbrn 0.01539942



# subset gss_clean to new dataframe with significant variables (remove those under 0.1)
gssNEW <- subset(gss_clean, select=c(coninc, age, childs, conlegis, degree,  
                              educ, finalter, granborn, homosex,  incom16,   marital,  owngun,  
                              race, region, satfin, sei, sex, speduc, spwrksta, tvhours, wrkstat,  year))
                                



# first try to impute with missForest
# library(missForest)
# missForest(gssNEW, maxiter = 10, ntree = 100)

# imute missing values with mice 00:20 (~3 hours to impute)

library(mice)
set.seed(88)
impute_gss = complete(mice( gssNEW ))

# random split to train and test set
require(caTools)
set.seed(101) 
sample = sample.split(impute_gss$coninc, SplitRatio = 0.7)
train = subset(impute_gss, sample == TRUE)
test = subset(impute_gss, sample == FALSE)

# save train and test set to folder
write.table(train, file = "train.csv", sep = ",")
write.table(test, file = "test.csv", sep = ",")

########### LM on imputed dataset

modelLM = lm(coninc~ log(age) + satfin + speduc + spwrksta + educ + sei + wrkstat +
               year + marital + degree + finalter + childs + region + sex, data=train)
              #owngun + race + incom16 + homosex
summary(modelLM)

predLM = predict(modelLM, newdata=test)

SSE = sum((test$coninc - predLM)^2)
SST = sum((test$coninc - mean(train$coninc))^2)
1 - SSE/SST
# 0.4327568

d <- ggplot(impute_gss,aes(x = age, y = coninc, colour=satfin)) + xlab("Age") + ylab("Income") + geom_point(size=2.5) + 
                      theme(text = element_text(size=14),legend.position="top",
                      axis.text.x = element_text(angle=90, vjust=1))
d + scale_fill_manual(values=c("#F8766D", "#00BA38", "#0000ff"))

######## Step LM

library(ggplot2)
ggplot(impute_gss, aes(age,coninc)) + geom_point() + geom_smooth()

plot(impute_gss$marital, impute_gss$coninc)