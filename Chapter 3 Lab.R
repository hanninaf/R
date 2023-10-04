# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR)
Advertising<-read.csv("Advertising.csv",head=TRUE)[,-1]

#Advertising data
#sales (in thousands of units) for a particular product 
#advertising budgets (in thousands of dollars) for TV, radio, and newspaper media
#On the basis of this data, suggest a marketing plan for next year 
#that will result in high product sales. 

#For .Rmd file, one need to specify the whole directory and file name as follows
wd <- getwd(); file <- paste(wd,"Advertising.csv",sep="/")
Advertising<-read.csv(file, head=TRUE)[,-1]
#One needs to use the full file name with location for reproducible research


#Preliminary work

head(Advertising)
summary(Advertising)
x11();pairs(Advertising, pch="o") #What do you see?

#A few important questions that we might seek to address.

#1.	Is there a relationship between advertising sales and budget?

# a multiple regression model of sales onto TV, radio, and newspaper
ad.lm <- lm(Sales~., data=Advertising)
names(summary(ad.lm))

# Test hypothesis H0 : beat_TV  = beta_radio  = beta_newspaper  = 0.  
# F-test, P-value < 2.2e-16
# F-test indicates clear evidence of a relationship between advertising 
# and  sales 
#summary(ad.lm)$fstatistic 
sum.ad.lm <- summary(ad.lm)
#pvalue=1-pf(sum.ad.lm$fstatistic[1],sum.ad.lm$fstatistic[2],sum.ad.lm$fstatistic[3])

`r pvalue`
#2.	How strong is the relationship?
names(summary(ad.lm))

#two measures of model accuracy:RSE and R^2
#RSE: Residual (y-yhat) standard error 
rse=summary(ad.lm)$sigma

#other way to get rse
sqrt(sum((ad.lm$residuals)^2)/(200-4))
  
#RSE= 1.686 
mean(Advertising$Sales) # 14.0225
rse/mean(Advertising$Sales) # noisy/signal=.12 
#percentage error wrt mean is roughly 12 %. 
#

rsq=summary(ad.lm)$r.sq
rsq #0.8972106
#The predictors explain almost 90 % of the variance in sales
# rsq is calculated by the following formuala
yhat=ad.lm$fitted.values #predicted 
y=Advertising$Sales #observed 
rsq=1-sum((y-yhat)^2)/sum((y-mean(y))^2) #orginal formula 


#Other way to get R2
var(yhat)/var(y) #other formula
1-sum((y-yhat)^2)/sum((y-mean(y))^2) #orginal formula 
cor(yhat,y)^2 #alternate formula
#They are equal for linear regression model.


#3.	Which media contribute to sales?
Coef1=summary(ad.lm)$coefficients #Coefficient matrix
Coef1
#Examining the p-values associated with each predictor's t-statistic, 
#the p-values for TV and radio are low, but the p-value for newspaper 
#is not. This suggests that only TV and radio are related to sales. 

#4.	How large is the e???ect of each medium on sales?
lolim=Coef1[,1] - 1.96*Coef1[,2]
uplim=Coef1[,1] + 1.96*Coef1[,2]
cbind(lolim,uplim)
confint(ad.lm)

#the 95 % confidence intervals are as follows: 
#(0.043, 0.049) for TV, 
#(0.172, 0.206) for radio, 
#(-0.013, 0.011) for newspaper. 

#The confidence intervals for TV and radio are narrow and far from zero, 
#providing evidence that these media are related to sales. 
#But the interval for newspaper includes zero, indicating that the 
#variable is not statistically signi???cant given the values of TV and 
#radio.

# Could collinearity be the reason that the con???dence in- terval 
# associated with newspaper is so wide? 
# Variation Inflation factor (vif) measures collinearity:
# vif(betaj hat)=1/(1-R^2) where R^2 is from the regression of Xj
# all the other predictors. 
# Rule of thumb: vif>5 or 10 indicates problematic collinearity.

install.packages("car");require(car)
vif(ad.lm) 

# The VIF scores are 1.005, 1.145, and 1.145 for TV, radio, and 
# newspaper, suggesting no evidence of collinearity.

# 5.	How accurately can we predict future sales?

#For individual  response, we use a prediction interval, 
#and for the  average  response,  f(X)  

#for the  average  response f(X)
predict(ad.lm, newdata=data.frame(TV=149,Radio=22,Newspaper=25),
        interval="confidence")

#for individual  response
predict(ad.lm, newdata=data.frame(TV=149,Radio=22,Newspaper=25),
        interval="prediction")

#Prediction  intervals are  always  wider  than  con???dence  
#intervals  because  they  account  for  the  uncertainty associated 
#with  epsilon e, the  irreducible error.

# 6.	Is the relationship linear?

plot(ad.lm)#diagnostic plot
#If the relationships are linear, then the residual plots 
#should display no pattern. 
#gam model in later chapters will do better

# 7.	Is there synergy among the advertising media?

#non-additive relationships model
ad.lm2 <- lm(Sales~.^2, data=Advertising)
summary(ad.lm2)

#the Advertising data  may not be additive.
summary(ad.lm2)$r.sq;summary(ad.lm)$r.sq 

#Including an interaction term in the model results in 
#a substantial increase in R2, from around 90 % to almost 97 %.

# Non-linear Transformations of the Predictors

ad.lm3 <- lm(Sales~.+I(TV^2), data=Advertising)
summary(ad.lm3)
anova(ad.lm,ad.lm3)
#Indicates the non-linear effect of TV

par(mfrow=c(2,2))
plot(ad.lm3)

ad.lm4 <- lm(Sales~.+poly(TV,3), data=Advertising)
summary(ad.lm4)
anova(ad.lm,ad.lm4)
anova(ad.lm3,ad.lm4)

par(mfrow=c(2,2))
plot(ad.lm4)

ad.lm5 <- lm(Sales~.+poly(TV,3)+poly(Radio,3), data=Advertising)
plot(ad.lm5)
anova(ad.lm4,ad.lm5)

#knn regression
install.packages("FNN")
require(FNN)
 
#k Nearest Neighbor Regression
trainx=Advertising[,-4] #X-matrix
ad.knn <- knn.reg(trainx, test = testx, Advertising$Sales, k = 1)
plot(Advertising$Sales,ad.knn$pred, xlab="y", ylab=expression(hat(y)))

#R^2
var(ad.knn$pred)/var(Advertising$Sales) 
#This formula may not work for multiple regression or other models
y=Advertising$Sales
yhat=ad.knn$pred
rsq=1-sum((y-yhat)^2)/sum((y-mean(y))^2);rsq
cor(yhat,y)^2 #approximate rsq very well. 

#Comparable to (ad.lm)? 
yhat2=predict(ad.lm,Advertising)
plot(Advertising$Sales,yhat2,xlab="y",ylab=expression(hat(y)))
rsq2=1-sum((y-yhat2)^2)/sum((y-mean(y))^2);rsq2
cor(yhat2,y)^2 #approximate rsq very w
#Need to apply this to the test data not training data


