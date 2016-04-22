#details for each exercise are in Exercise.pdf

#### Exercise 1.1.

#Which option is closer to the mean of Y? a) or b)?

exp(0+1/2) #a)
#1.648721

exp(0-1/2) #b)
#0.6065307

x <- rnorm(1000)
y <- exp(x)

mean(y)
#1.687353

#Answer: a)

#### Exercise 1.2.

#Consider the second order polynomial equation: x^2 + 3x + 1 = 0

#The general polynomial equation AX^2 + BX + C = 0
#has the solutions (-B +/- sqrt(B^2 - 4AC))/2A

#1.Construct a vector of length 2 that contains the solutions to the equation 
#x^2 + 3x + 1 = 0 and display it on the screen with 1 decimal point. 

A <- 1
B <- 3
C <- 1

options(digits=1)
c ( (-B+sqrt(B^2 - 4*A*C))/(2*A),
    (-B-sqrt(B^2 - 4*A*C))/(2*A)
  )

#[1] -0.4 -2.6

#2. Work out how much error you make (in percent) by referring to the solutions with 
#just one decimal. The result will be different for the two solutions.

options(digits=6)
100*(c(-0.4, -2.6)/c ( (-B+sqrt(B^2 - 4*A*C))/(2*A),
                 (-B-sqrt(B^2 - 4*A*C))/(2*A)
))

#104.7214  99.3112

# if we use only one ticket we only accelerate 1st solution by 4,7% (104.7214)

#### Exercise 1.3.

#Construct the object x as
set.seed(1234)
x <- rnorm(100, mean=.5, sd=.3)

#Perform the following task (check out the help system as needed): 
#1.Calculate mean and standard deviation of x.
mean(x)
sd(x)

#2.Plot a histogram of x.
hist(x)

#3.Put the second axis on the right side of the histogram plot instead of on the left.
hist(x)
axis(1)
axis(4)