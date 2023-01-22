#Authors:  Andrej Erkelens and Carol Moore; Revised by Carol Moore May 2021
setwd('/Documents/R files/STAT/classdata')
diamonds4 <- read.csv("diamonds4.csv")
attach(diamonds4)
library(MASS) #for box-cox plot

## ASSESS IF LINEAR REGRESSION ASSUMPTIONS ARE MET AND MAKE NECESSARY TRANSORMATIONS
##########################
# Examine relationship between price and carat
plot(carat,price, main="Plot of Price Against Carat",ylab="Price ($)",xlab='Carat')
# Exponential increase

# Baseline performance simple linear model
result = lm(price~carat)
plot(result$fitted.values,result$residuals, xlab="Fitted Values", ylab="Residuals", main="Residual Plot:\n Simple Price-Carat Model")
# Severe heteroskedasticity

# Log transformation of response and predictor
trans.price = log(price) 
trans.carat = log(carat) ### Also will take a natural log due to the spread of the carat values
plot(trans.carat,trans.price,main="Plot of Price Against Carat 
     \n with Log Transformation",ylab="Log Price ($)",xlab='Log Carat') ### Linear

updated = lm(trans.price~trans.carat)
plot(updated$fitted.values,updated$residuals, xlab="Fitted Values", ylab="Residuals", main="Residual Plot\n logprice, logcarat")
abline(h=0,col='red') 
# Error terms have constant variance

# Additional tests of transformed model
acf(updated$residuals,main="Autocorrelation Function ") ### No autocorrelation
qqnorm(updated$residuals) 
qqline(updated$residuals)  #reasonably normal
boxcox(updated) #lamba = 1 outside 95% CI, raise to 1/3 power

# ADDING "CLARITY" TO THE MODEL
##########################
# Recoding Clarity 
diamonds4$clarity_cat[clarity=="FL"|clarity=="IF"] <- "flawless"
diamonds4$clarity_cat[clarity=="VVS1"|clarity=="VVS2"] <- "near_flawless"
diamonds4$clarity_cat[clarity=="VS1"|clarity=="VS2"] <- "very_slight_flaw"
diamonds4$clarity_cat[clarity=="SI1"|clarity=="SI2"] <- "slight_flaw"
# Set factor
is.factor(diamonds4$clarity_cat)
diamonds4$clarity_cat = factor(diamonds4$clarity_cat)
## Slight Flaw as the Reference
diamonds4$clarity_cat<-relevel(diamonds4$clarity_cat, ref = "slight_flaw")

# Check significance as predictor
test1 = lm(trans.price ~ trans.carat + diamonds4$clarity_cat)
anova(updated,test1) # Partial F 
# Since F is significant we reject the null and we cannot drop this variable from our regression.
# New model is log(price) = log(carat) + clarity_cat

#ADDING COLOR TO THE MODEL
##########################
# Recode
diamonds4$color_cat[color=="D"] <- "perfectlyclear"
diamonds4$color_cat[color=="E"| color=="F"] <- "clear"
diamonds4$color_cat[color=="G"| color=="H"] <- "veryslightcolor"
diamonds4$color_cat[color=="I"| color=="J"] <- "slightcolor"
diamonds4$color_cat = factor(diamonds4$color_cat)
## Slight Color Is the Reference
diamonds4$color_cat<-relevel(diamonds4$color_cat, ref = "slightcolor")

# Test for significance of added variables in partial F-test 
test2 = lm(trans.price ~ trans.carat +  diamonds4$clarity_cat + diamonds4$color_cat)
anova(test1,test2)# F is significant therefore we retain variable "color cat"
# Check the overall regression for p values 
summary(test2)

#ADDING CUT TO THE MODEL
##########################
levels(cut)
diamonds4$cut = factor(diamonds4$cut) #Use the original categories
## "Good" cut is the reference
diamonds4$cut<-relevel(diamonds4$cut, ref = "Good")
# Test for significance of added variables in partial F-test
test3 = lm(trans.price ~ trans.carat + diamonds4$clarity_cat+ diamonds4$color_cat  + diamonds4$cut)
anova(test2,test3)# F is significant therefore we retain var
# Check the overall regression for p values 
summary(test3)
# All significant in t test, R2 .98

# DIAGNOSTIC TESTS WITH ALL CARAT, CUT, COLOR, CLARITY - NO INTERACTIONS

par(mfrow=c(2,2))
plot(test3$fitted.values,test3$residuals, xlab="Fitted Values", ylab="Residuals", main="Residual Plot\n logprice, logcarat")
abline(h=0,col='red') ### Error terms look good (variance)
acf(test3$residuals,main="Autocorrelation Function ") ### The ACF plot looks good for each lag after 0 which further shows the variance assumption is not violated
qqnorm(test3$residuals) 
qqline(test3$residuals)
boxcox(test3) ##Need to adjust response var to be logprice**.3

# VARIATIONS ON MODEL
#Model2:  basic model - Model 1 With logprice**.3
tlogprice=trans.price**.3
bmod<-lm(tlogprice~trans.carat+diamonds4$cut+diamonds4$clarity_cat+diamonds4$color_cat)
plot(bmod$fitted.values,bmod$residuals, xlab="Fitted Values", ylab="Residuals", main="Residual Plot\n logprice, logcarat")
abline(h=0,col='red') ### Error terms look good (variance)
acf(bmod$residuals,main="Autocorrelation Function ") ### The ACF plot looks good for each lag after 0 which further shows the variance assumption is not violated
qqnorm(bmod$residuals) 
qqline(bmod$residuals)
anova(bmod)
summary(bmod)

#Model3:  maxmod - All categorical vars interacted with carat;  logprice**.3 is response
maxmod<-lm(tlogprice~trans.carat*diamonds4$cut+trans.carat*diamonds4$clarity_cat+trans.carat*diamonds4$color_cat)

par(mfrow=c(2,2))
plot(maxmod$fitted.values,maxmod$residuals, xlab="Fitted Values", ylab="Residuals", main="Residual Plot")
abline(h=0,col='red') ### Error terms look good (variance)
acf(maxmod$residuals,main="Autocorrelation Function ") 
qqnorm(maxmod$residuals) 
qqline(maxmod$residuals)

anova(maxmod,bmod)
anova(maxmod)
summary(maxmod)

#Model4:  nocutmod - Drop interactions for cut due to relatively high multicollinearity

nocutmod<-lm(tlogprice~trans.carat+diamonds4$cut+trans.carat*diamonds4$clarity_cat+trans.carat*diamonds4$color_cat)
summary(nocutmod)

par(mfrow=c(2,2))
plot(nocutmod$fitted.values,nocutmod$residuals, xlab="Fitted Values", ylab="Residuals", main="Residual Plot")
abline(h=0,col='red') ### Error terms look good (variance)
acf(nocutmod$residuals,main="Autocorrelation Function ") ### The ACF plot looks good for each lag after 0 which further shows the variance assumption is not violated
qqnorm(nocutmod$residuals) 
qqline(nocutmod$residuals)

anova(nocutmod,maxmod)
summary(nocutmod)

# Model 5 - maxmod2 - For comparison, use logprice as response var instead of logprice**.3

maxmod2<-lm(trans.price~trans.carat*diamonds4$cut+trans.carat*diamonds4$clarity_cat+trans.carat*diamonds4$color_cat)
summary(maxmod2)
meancar = mean(trans.carat)

#Prediction for logprice **.3 model
newdata<-data.frame(cut,trans.carat=meancar,diamonds4$clarity_cat=="slight_flaw",diamonds4$color_cat=="slightcolor")
predict1=predict.lm(maxmod,newdata,level=0.95, interval="prediction")
summary(predict1)
aggregate(predict1,list(cut), mean)

#Prediction for logprice model
newdata<-data.frame(cut,trans.carat=meancar,diamonds4$clarity_cat=="slight_flaw",diamonds4$color_cat=="slightcolor")
predict2=predict.lm(maxmod2,newdata,level=0.95, interval="prediction")
summary(predict2)
aggregate(predict2,list(cut), mean)

##THE MODEL FOR HYPOTHESIS TESTING IS MODEL 5 - NOCUT, logprice**.3
summary(nocutmod)
vcov(nocutmod)

#Predicted values for table
newdata<-data.frame(cut,trans.carat=meancar,diamonds4$clarity_cat=="slight_flaw",diamonds4$color_cat=="slightcolor")
predict3=predict.lm(nocutmod,newdata,level=0.95, interval="confidence")
summary(predict3)
aggregate(predict3,list(cut), mean)
detach(diamonds4)
