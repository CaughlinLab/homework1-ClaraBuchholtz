#Modeling Class Homework 1
setwd("~/Desktop/Modeling Class/homework1")

#-----Question 1-----#
#Read in math scores data
math<-read.csv("math_scores.csv")
#a) a scatterplot of the dependent and indepedent variable with a 
#line added (using curve) representing best fit least squares model
plot(math$MATH_score ~ math$LSD_concentration)
modelMS<- lm(math$MATH_score ~ math$LSD_concentration)
interceptMS<-89.124
slopeMS<--9.009
#add line
curve(interceptMS+slopeMS*x, add=T)
#b) Parameter estimates and 95% CI for slope and intercept parameters
#95% confidence interval
confint(modelMS)
#c) A metric of model fit (calculate either R2 or RMSE using the 
#functions above) reminder: y_hat is calculated using the equation 
#for a line applied to the predictor variable
#Calculate r^2 and RMSE
#predicted values:
yhat_MS <- interceptMS+slopeMS*math$LSD_concentration
#actual values
yMS <- math$MATH_score
#R^2 function from Hmwk 1
#rmse
rmse=function(y_hat,y)
{
  return(sqrt(mean((y-y_hat)^2))) }
rmse(yhat_MS,yMS)

#r^2
#r^2
r2<-function(y_hat,y) {
  RSS<-sum((((y_hat))-(y))^2)
  TSS<-sum(((y)-(mean(y)))^2)
  return(1-RSS/TSS)}
r2(yhat_MS, math$MATH_score)

#A. What level of LSD tissue concentration do you need to
#ensure a test score of >85%? 
LSDconForScore=function(score)
{
  return((score-interceptMS)/slopeMS)
}
LSDconForScore(85)


#-------Question 2--------#
#Read in and plot miracle food data
miracle<-read.csv("miracle_food.csv")
#a) a scatterplot of the dependent and indepedent variable with a 
#line added (using curve) representing best fit least squares model
plot(miracle$Weight_loss ~ miracle$pomegranate)
modelWL<-lm(miracle$Weight_loss ~ miracle$pomegranate)
interceptWL<--0.1790
slopeWL<--0.5251
curve(interceptWL+slopeWL*x, add=T)
#b) Parameter estimates and 95% CI for slope and intercept parameters
#95% confidence interval
confint(modelWL)
#c) A metric of model fit (calculate either R2 or RMSE using the 
#functions above) reminder: y_hat is calculated using the equation 
#for a line applied to the predictor variable
#Calculate r^2 and RMSE
#predicted y values given model parameters
yhat_WL<-interceptWL+slopeWL*miracle$pomegranate
#actual y values
yWL<-miracle$Weight_loss
#R^2 function from Hmwk 1 (rmse)
rmse=function(y_hat,y)
{
  return(sqrt(mean((y-y_hat)^2))) }
rmse(yhat_WL,yWL)
#r^2
r2<-function(y_hat,y) {
  RSS<-sum((((y_hat))-(y))^2)
  TSS<-sum(((y)-(mean(y)))^2)
  return(1-RSS/TSS)}
r2(yhat_WL, miracle$Weight_loss)


#----Question 3---#
#A. Translate the mathematical equation for MAE into a function in R.
MeanAbs<- function(y, yhat) {
 return((1/length(y))*sum((abs(y-yhat)))) 
}

#MAE for the math scores
MeanAbs(math$MATH_score, yhat_MS)
# 4.890816

#MAE for the Miracle pomegranate data
MeanAbs(miracle$Weight_loss, yhat_WL)
# 7.981461


#Alternatively, also this:
install.packages('Metrics')
library(Metrics)
mae(miracle$Weight_loss, yhat_WL)


#----Question 4----#
#Simulate linear data.

#Step 1: Create a predictor variable using runif or seq
#Predictor variable: 
#NumKittens,  number of cute kitten pics an account has on Instagram
NumKittens<-seq(300)

#Step 2: Decide on a value for the intercept and slope

slopeK<-20
interceptK<-15
sigmaK<-50

# Step 3: Use rnorm to simulate draws from a normal distribution for your dataset
NumLikes <- rnorm(300, mean=interceptK + slopeK*NumKittens, sd=50)
hist(NumLikes)
# A. Plot your data
 plot(NumLikes~NumKittens)
# B. Estimate the slope and intercept parameters 
# from the data using linear regression. 
kittenMod<-lm(NumLikes~NumKittens)
kittenMod
# Coefficients:
#   (Intercept)   NumKittens  
# 9.378        20.028


#------Question 5----------#

# Unequal variance (heteroskedasticity) between subgroups 
# of the response variable violates assumptions of least squares. 
# Using rnorm and steps above in Question 5, simulate a normally-distributed
# random variable where the variance of the response variable 
# increases as a function of a predictor variable.

#Step 1: Create a predictor variable using runif or seq
#Scenario: number of cats Clara will have as she transforms into an extreme 
#cat lady and lives to an unlikely old age. However the further
#into the future we look the less certain we can be about
#her capacity for more cats... 
#Predictor variable: Age
ClaraAge<-seq(150)

#Step 2: Decide on a value for the intercept and slope

slopeC<-.3
interceptC<-0
sigmaC<-.2*ClaraAge

# Step 3: Use rnorm to simulate draws from a normal distribution for your dataset
NumCats <- rnorm(150, mean=interceptC + slopeC*ClaraAge, sd=sigmaC)
hist(NumCats)

# A. Construct a plot showing your simulated data
plot(NumCats~ClaraAge)
 
# B. What is a potential biological explanation for the data you have simulated?