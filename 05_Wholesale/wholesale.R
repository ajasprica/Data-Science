
#The data set refers to clients of a wholesale distributor. It includes the annual spending in monetary units (m.u.) on diverse product categories


#1) FRESH: annual spending (m.u.) on fresh products (Continuous);
#2) MILK: annual spending (m.u.) on milk products (Continuous);
#3) GROCERY: annual spending (m.u.)on grocery products (Continuous);
#4) FROZEN: annual spending (m.u.)on frozen products (Continuous)
#5) DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous)
#6) DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous);
#7) CHANNEL: customers’ Channel - Horeca (Hotel/Restaurant/CafA�) or Retail channel (Nominal)
#8) REGION: customers’ Region – Lisnon, Oporto or Other (Nominal)

#Descriptive Statistics:
#(Minimum, Maximum, Mean, Std. Deviation)
#FRESH ( 3, 112151, 12000.30, 12647.329)
#MILK (55, 73498, 5796.27, 7380.377)
#GROCERY (3, 92780, 7951.28, 9503.163)
#FROZEN (25, 60869, 3071.93, 4854.673)
#DETERGENTS_PAPER (3, 40827, 2881.49, 4767.854)
#DELICATESSEN (3, 47943, 1524.87, 2820.106)

#REGION Frequency
#Lisbon 77
#Oporto 47
#Other Region 316
#Total 440

#CHANNEL Frequency
#Horeca 298
#Retail 142
#Total 440 

# Can i predict distribution channel based on selling?

data = read.csv("Wholesale customers data.csv", stringsAsFactors=FALSE)


# create copy of original data

sales <- data

table(sales$Channel)
table(sales$Region)

# set Horeca channel to be reference point since it have more observation
sales$Channel[sales$Channel == 1] <- 0
sales$Channel[sales$Channel == 2] <- 1

# set Other region to be reference point since it have more observation
sales$Region[sales$Region == 3] <- 0

#since Channel and Region are numeric variables, need to make them as charachter
sales$Channel = as.factor(sales$Channel)
sales$Region = as.factor(sales$Region)

require(caTools)
set.seed(101) 
sample = sample.split(sales$Channel, SplitRatio = 0.7)
train = subset(sales, sample == TRUE)
test = subset(sales, sample == FALSE)

###### Random Forest
library(randomForest)
modelRF = randomForest(Channel ~ ., data=train, method="class")

predRF = predict(modelRF, newdata = test, type="class")
table(test$Channel, predRF)
(82+39)/nrow(test)
# 0.9166667

###### CART 

library(rpart)
library(rpart.plot)
CART = rpart(Channel ~ ., data=train, method="class")

predCART = predict(CART, newdata = test, type="class")
table(test$Channel, predCART)
(83+39)/nrow(test)
# 0.9242424

###### SVM

library(e1071)

# perform a grid search
tuneResult <- tune(svm, Channel ~ .,  data = train,
                   ranges = list(epsilon = seq(0,0.1,0.01), cost = 2^(2:9))
)
print(tuneResult)

#- sampling method: 10-fold cross validation 
#- best parameters:
#  epsilon cost
#  0       16
#- best performance: 0.1009677
plot(tuneResult)

# Darker areas are better - C under 50
# automatic pick best model
tunedSVM <- tuneResult$best.model

#prediction on test set
predSVM <- predict(tunedSVM, test) 
table(test$Channel, predSVM)
(83+37)/nrow(test)
# 0.9090909

###########################################

# plot the correlation data
library(PerformanceAnalytics)
chart.Correlation(sales[sales$Channel==0, 3:8])
chart.Correlation(sales[sales$Channel==1, 3:8])

#t-test of Detergents_Paper

t.test(sales[sales$Channel==0, 7], sales[sales$Channel==1, 7])

#Welch Two Sample t-test

#data:  sales[sales$Channel == 0, 7] and sales[sales$Channel == 1, 7]
#t = -12.183, df = 145.15, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -7530.012 -5427.882
#sample estimates:
#  mean of x    mean of y 
#  790.5604     7269.5070

# Overlap Histogram Colored (blue and red)
hist(sales[sales$Channel==0, 7], col=rgb(1,0,0,0.5), xlim=c(0,40000), ylim=c(0,150), main="", xlab="Detergents and Paper")
par(new=TRUE)
hist(sales[sales$Channel==1, 7], col=rgb(0,0,1,0.5), xlim=c(0,40000), ylim=c(0,150), main="", xlab="")