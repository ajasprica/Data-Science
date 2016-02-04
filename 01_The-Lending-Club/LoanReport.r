

### Libraries

library(RCurl) #read data from web
library(ggplot2) #advanced graphic package
library(caret) #model training and parameter tuning
library(randomForest) #classification and regression tree
library(caTools) #use to random split data in train/test set
library(MASS) #functions
library(plyr) #data transformation
library(DT) #datatable in Marketing section
library(e1071) #library for SVM
library(rattle) #library for Data Mining task
library(glmnet) #use for cross validation

### Data Processing

#read data from "Data Analysis"" course
loansURL = read.csv(text=getURL("http://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv"))

#removes 2 rows with missing data
loansURL <- na.omit(loansURL)

#inspect data structure
str(loansURL)


#On first sight i notice how Interest.Rate variable is factor with 275 and Debt.To.Income.Ratio is factor with 1669 levels so i start with assumption that this two variables need to transformed to numeric, so i removed "%" sign and transform them to numeric. Also transform FICO score from from factor to numeric on way that range was transformed to average value. For example "640-644" is 642 so i could experiment with both factor and numeric predictions of independent variable. 
#On start i create copy of original dataset because if something happen while working with data i can easily recall it from original dataset.

#create copy of original data
loans <- loansURL

# remove % from Interest.Rate and Debt.To.Income.Ratio
loans$Interest.Rate <- gsub("%", "", loans$Interest.Rate)
loans$Debt.To.Income.Ratio <- gsub("%", "", loans$Debt.To.Income.Ratio)

# turn factor to numeric
loans$Interest.Rate = as.numeric(loans$Interest.Rate)
loans$Debt.To.Income.Ratio <- as.numeric(loans$Debt.To.Income.Ratio)

# transform from fisco range to average fisco value; 
FICO.function = function(x) {
(as.numeric(substr(x, 0, 3)) + as.numeric(substr(x, 5, 8)))/2
}
loans$FICO.Mean = sapply(loans$FICO.Range, FICO.function)
loans$FICO.Mean = as.factor(loans$FICO.Mean)

#remove FISCO.Range from dataset and Amount.Funded.By.Investors since on they website is notice that interest rate don't depend on it
loans$FICO.Range <- NULL
loans$Amount.Funded.By.Investors <- NULL


#Root-mean-square error **(RMSE)** is a frequently used measure of the differences between values predicted by a model and the values actually observed and represents the sample standard deviation of the differences between predicted values and observed values. 
#These individual differences are called residuals when the calculations are performed over the data sample that was used for estimation, and are called prediction errors when computed out-of-sample.
#Here i create simple function to easily calculate RMSE.


# error can be calculated as "summary(model)$residuals" or out-of-sample as
# "loans$Interest.Rate - predModel" where predModel is calculated as
# "predModel <- predict(model, data=loans)"

rmse <- function(error)
{
  sqrt(mean(error^2))
}


#### K-MEANS CLUSTERING

#Prepare data for cluster analysis, create copy from loans dataframe
loansKM <- loans

#remove variables that are not going to be include
loansKM$State <- NULL
loansKM$Loan.Purpose <- NULL
loansKM$Home.Ownership <- NULL

#str(loansKM) - inspect data: need to make all variables as numeric
loansKM$Loan.Length <- gsub(" months", "", loansKM$Loan.Length)

#transform Employment.Lenght from factor to numeric
loansKM$Employment.Length<- gsub("n/a", "11", loansKM$Employment.Length)
loansKM$Employment.Length<- gsub(" years", "", loansKM$Employment.Length)
loansKM$Employment.Length<- gsub(" year", "", loansKM$Employment.Length)
loansKM$Employment.Length[loansKM$Employment.Length=="< 1"] <- 0
loansKM$Employment.Length[loansKM$Employment.Length=="10+"] <- 14
loansKM$Employment.Length[loansKM$Employment.Length=="11"] <- 0
loansKM$Employment.Length = as.numeric(loansKM$Employment.Length)

#transform other variables to numeric
loansKM$Amount.Requested = as.numeric(loansKM$Amount.Requested)
loansKM$Loan.Length = as.numeric(loansKM$Loan.Length)
loansKM$FICO.Mean = as.numeric(levels(loans$FICO.Mean))[loans$FICO.Mean]
loansKM$Open.CREDIT.Lines = as.numeric(loansKM$Open.CREDIT.Lines)
loansKM$Revolving.CREDIT.Balance = as.numeric(loansKM$Revolving.CREDIT.Balance)
loansKM$Inquiries.in.the.Last.6.Months = as.numeric(loansKM$Inquiries.in.the.Last.6.Months)


#Inspect cleaned data:
  
str(loansKM)


#Normalize the variables in the loansKM data frame by using the preProcess function in the "caret" package

# Pre process
preproc = preProcess(loansKM)
# Normalize
loansNorm = predict(preproc, loansKM)
# Inspect normalized data:
str(loansNorm)


#K means clustering

set.seed(88)
#preform k-means clustering
KmeansCluster <- kmeans(loansNorm, centers = 10)
#extracts clusters
loansClusters <- KmeansCluster$cluster
# distribution of clusters
table(loansClusters)

KmeansCluster$centers


#Calculate 


Amount.Req = tapply(loansKM$Amount.Requested, loansClusters, mean)
Int.Rate = tapply(loansKM$Interest.Rate, loansClusters, mean)
Loan.Length = tapply(loansKM$Loan.Length, loansClusters, mean)
Debt.To.Income = tapply(loansKM$Debt.To.Income.Ratio, loansClusters, mean)
Mon.Income = tapply(loansKM$Monthly.Income, loansClusters, mean)
Open.Lines = tapply(loansKM$Open.CREDIT.Lines, loansClusters, mean)
Balance = tapply(loansKM$Revolving.CREDIT.Balance, loansClusters, mean)
Inquiries = tapply(loansKM$Inquiries.in.the.Last.6.Months, loansClusters, mean)
Employ.Length = tapply(loansKM$Employment.Length, loansClusters, mean)
FICO = tapply(loansKM$FICO.Mean, loansClusters, mean)

avgTable <- as.data.frame(cbind(Int.Rate, FICO, Mon.Income, Amount.Req, Loan.Length, Debt.To.Income, Open.Lines, Balance, Inquiries, Employ.Length))


avgTable


cluster1 = subset(loans, loansClusters==1)
cluster2 = subset(loans, loansClusters==2)
cluster3 = subset(loans, loansClusters==3)
cluster4 = subset(loans, loansClusters==4)
cluster5 = subset(loans, loansClusters==5)
cluster6 = subset(loans, loansClusters==6)
cluster7 = subset(loans, loansClusters==7)
cluster8 = subset(loans, loansClusters==8)
cluster9 = subset(loans, loansClusters==9)
cluster10 = subset(loans, loansClusters==10)


#someone without programming background want to explore cluster on his own to make conclusions, but he doesn't know how to program in R. So i create new dataset called marketing based on variables he want to explore.


marketing = cbind(loans, loansClusters)
marketing = marketing[,c(9,6,11,13,12,14)]
marketing$loansClusters = as.factor(marketing$loansClusters)


#Then i add small datatable applet from "DT" package:

datatable(marketing, filter = 'top', class = 'cell-border stripe', rownames = FALSE)


### Exploratory data analysis

#### Random Forest: Variable importance


set.seed(144)
RF = randomForest(Interest.Rate ~ ., data=loans, importance = TRUE)
varImpPlot(RF, type=1, main="Variable importance")
importance(RF)
print(RF)


#### How Interest rate is related to FICO score and Loan Lenght?


coef(lm(Interest.Rate~as.numeric(levels(FICO.Mean))[FICO.Mean], data=loans))

# geom_point(size=2.5) - define dots on graph of size 2.5
# theme() - define theme of graph
# element_text(size=14) - size of text on graph
# legend.position="top" - puts legend on top "Loan Lenght: 36/60 months"
# axis.text.x - text on x axis; angle 90
# scale_fill_manual() - fill graph with colors (by months)
# geom_abline() - create linear regression line

d <- ggplot(loans,aes(x = as.numeric(levels(FICO.Mean))[FICO.Mean], y = Interest.Rate, colour=Loan.Length)) + xlab("FICO Range") + ylab("Interest Rate (%)") + geom_point(size=2.5) + theme(text = element_text(size=14),legend.position="top",
axis.text.x = element_text(angle=90, vjust=1))
d + scale_fill_manual(values=c("#F8766D", "#00BA38")) + geom_abline(intercept = 73, slope = -0.085)



#### How is loan purpose related to interest rate?
#In 1307 cases reason for credit is debt consolidation, which means that borrowers raise one loan to pay-off many small loans. By debt consolidation, borrowers might have more favorable and lower interest rate.


sort(table(loans$Loan.Purpose), decreasing = TRUE)


boxplot(as.numeric(loans$Interest.Rate) ~ loans$Loan.Purpose, ylab = "Interest Rate", main = "Interest Rate vs. Loan Purpose", las=2)


#### How countries are represented in dataset?

sort(table(loans$State), decreasing = TRUE)

#California and New York are two most represented countries and together make 27% loans in dataset.

#### Monthly Income and Revolving CREDIT Balance log(x) transformation

par(mfrow=c(1,2))
hist(loans$Monthly.Income, col="blue", xlab="Monthly Income", main="")
hist(log(loans$Monthly.Income), col="blue", xlab="Transformation log(x)", main="log(x)")

anova(lm(Monthly.Income ~ ., loans))



#Who is borrower with minimum monthly income?


which.min(loans$Monthly.Income)
loans[1685,]


#Who is borrower with maximum monthly income?


which.max(loans$Monthly.Income)
loans[1850,]

par(mfrow=c(1,2))
hist(loans$Revolving.CREDIT.Balance, col="blue", xlab="Revolving CREDIT Balance", main="")
hist(log1p(loans$Revolving.CREDIT.Balance), col="blue", xlab="log1p(Revolving CREDIT Balance)", main="Transformation log(1+x)")

anova(lm(Revolving.CREDIT.Balance ~ ., loans))


#### Linear Regression: Model selection

#set FICO as numeric as.numeric(FICO.Mean)
lmModel1 = lm(Interest.Rate ~ as.numeric(FICO.Mean) + Loan.Length + Amount.Requested + Debt.To.Income.Ratio + Inquiries.in.the.Last.6.Months + Open.CREDIT.Lines + Revolving.CREDIT.Balance + Home.Ownership + Loan.Purpose + Monthly.Income, data=loans)

errorLM1 = summary(lmModel1)$residuals
predLM1.RMSE <- rmse(errorLM1)

cat("lmModel1 | RMSE:", predLM1.RMSE, ", Multiple R-squared: ", summary(lmModel1)$r.squared, ", Adjusted R-squared: ", summary(lmModel1)$adj.r.squared)


#lmModel2: set FICO as factor *as.factor(FICO.Mean)*

lmModel2 = lm(Interest.Rate ~ as.factor(FICO.Mean) + Loan.Length + Amount.Requested + Debt.To.Income.Ratio + Inquiries.in.the.Last.6.Months + Open.CREDIT.Lines + Revolving.CREDIT.Balance + Home.Ownership + Loan.Purpose + Monthly.Income, data=loans)

errorLM2 = summary(lmModel2)$residuals
predLM2.RMSE <- rmse(errorLM2)

cat("lmModel2 | RMSE:", predLM2.RMSE, ", Multiple R-squared: ", summary(lmModel2)$r.squared, ", Adjusted R-squared: ", summary(lmModel2)$adj.r.squared)



#lmModel3: simple model

lmModel3 = lm(Interest.Rate ~ as.factor(FICO.Mean) + Loan.Length + Amount.Requested + Monthly.Income, data=loans)

errorLM3 = summary(lmModel3)$residuals
predLM3.RMSE <- rmse(errorLM3)

cat("lmModel3 | RMSE:", predLM3.RMSE, ", Multiple R-squared: ", summary(lmModel3)$r.squared, ", Adjusted R-squared: ", summary(lmModel3)$adj.r.squared)



#lmModel4:

matrixModel4 <- model.matrix(Interest.Rate ~ log(Monthly.Income)*(Amount.Requested + Loan.Length + Debt.To.Income.Ratio + Open.CREDIT.Lines + Revolving.CREDIT.Balance)^2 + Loan.Purpose + Home.Ownership + Employment.Length + FICO.Mean, data=loans)[,-1]

dim(matrixModel4)

lmModel4 = lm(Interest.Rate ~ Monthly.Income*(Amount.Requested + Loan.Length + Debt.To.Income.Ratio + Revolving.CREDIT.Balance)^2 + Loan.Purpose + Home.Ownership + Employment.Length + FICO.Mean + Open.CREDIT.Lines, data=loans)

errorLM4 = summary(lmModel4)$residuals
predLM4.RMSE <- rmse(errorLM4)

cat("lmModel4 | RMSE:", predLM4.RMSE, ", Multiple R-squared: ", summary(lmModel4)$r.squared, ", Adjusted R-squared: ", summary(lmModel4)$adj.r.squared)

step.fit = step(lmModel4, direction = "backward", trace = 1)
summary(step.fit)


#### Diagnostic of linear regression model

#Plot results of lmModel3 and show 45 degree line

predLM3 = predict(lmModel3, data=loans)
plot(loans$Interest.Rate, predLM3, xlab="Actual", ylab="Predicted")
abline(0,1,col="red")



#Linear Regression: Train and test set split

set.seed(201)
spl = sample.split(loans$FICO.Mean, 0.8)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)

lmModel = lm(Interest.Rate ~ as.factor(FICO.Mean) + Loan.Length + Amount.Requested + Monthly.Income, data=train)

cat("lmModel R^2 on train set is: ", summary(lmModel)$r.squared)

#prediction on test set and calculate R^2
predLm = predict(lmModel, newdata = test)
SSE = sum((predLm-test$Interest.Rate)^2)
baseline = mean(train$Interest.Rate) 
SST = sum((baseline-test$Interest.Rate)^2)
Rsqr = 1 - SSE/SST
cat("Linear regression R^2 on test set is: ", Rsqr)

kappa(lmModel3)


par(mfrow=c(2,2))
plot(lmModel)



#### Two Sample Proportion: Mortage vs Rent

#In our dataset there are 1145 borrowers that rent home and 1148 that own mortgage. So i start to investigate further differences between two groups and how they effect Interest.Rate. 


#subset loans dataset to mortage and rent dataset
mortgage = subset(loans, Home.Ownership =="MORTGAGE") #1148 observations
rent = subset(loans, Home.Ownership =="RENT") #1145 observations

#create two tables and store it to variable
Employment.Rent = as.data.frame.table(table(rent$Employment.Length))
Employment.Mortgage = as.data.frame.table(table(mortgage$Employment.Length))

#merge two tables by variable "Var1"
employment = merge(Employment.Rent, Employment.Mortgage, by="Var1") 
rownames(employment) <- as.character(employment$Var1) #use vector as colum names
employment = employment[,-1] #drops Var1 since we don't need it
employment = t(employment) # swap rows and colums
employment = employment[,c(1,2,4,5,6,7,8,9,10,11,3,12)] #reorder colums so that years are ordered on right way



#After plotting data we see that those who work 6 years and less on current job prefer renting, while those above 6 years prefer mortgage.

barplot(employment, col=c("red", "yellow"), width=2, beside=TRUE, las=2, ylab="Frequencies", main="Years on current job vs Mortgage/Rent")
legend("top",inset=c(-0.25,0), title="Borrowers:", legend = c("Rent: 1145", "Mortgage: 1148"), fill=c("red", "yellow"))

#Let's test null hypothesis 
#H0: there is no difference between monthly income between rent and mortgage group.

#preform ANOVA test
anova.Owner = aov(Monthly.Income~Home.Ownership,data=loans) 
print(model.tables(anova.Owner,"means"),digits=3)


#preform t-test between mortage and rent dataset
t.test.Owner = t.test(mortgage$Monthly.Income, rent$Monthly.Income)
t.test.Owner


#Based on tests preformed we are rejecting null hypothesis and accept alternative hypothesis: true difference in means is not equal to 0. 
#Results can be interpreted that younger credit applicants didn't yet settled in life, have impulsive nature and less financial responsibilities towards family.

#This interpretation can have impacts on our Interest rate because we can threat those who rent as risk group. If we plot Mortgage vs Rent interest rate we see that those in Mortgage group have lower interest rates, but this could be reason because they have higher monthly income.


boxplot(mortgage$Interest.Rate, rent$Interest.Rate, names=c("Mortgage","Rent"), ylab="Interest Rate (%)", main="Interest rate on Mortgage and Rent subset")


### Conclusion

#Interest rate is dependent by FICO score, lenght of loan and amount requested. Several linear regression models achieved approximately 80% accuracy in predicting Interest rate of borrower. 
#By comparing lmModel1 who have numerical value of FICO score and lmModel2 who have factor value we conclude that sometimes we can increase accuracy if we turn numerical variable to factor variable, in our case by 4%.

