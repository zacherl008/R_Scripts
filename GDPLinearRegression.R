##This script runs on data regarding the Life Expectancy, GDP, and HIV rates per country.
##GDP vs Life Expectancy and GDP vs HIV rates are plotted and then a regresson line is added and then the coefficient of determination, ##confidence interval, R-Squared, and standard error are calculated from this.
##A hypothesis test is also run in order to determine the P value and if the correlation is statistically significant.

##Kyndra Zacherl
##Reading LifeGDPHiv data
data <- read.table('C://Users/kzacherl//Desktop/Rdata/LifeGDPhiv.txt')
##Naming the columns
colnames(data) <- c("CountryName", "LifeExpectancy", "GDP", "HIV")
attach(data)

###
##Compare life expectancy with GDP
##

##Initial scatter plot
plot(GDP, LifeExpectancy, main = "GDP vs Life Expectancy")

##Calculating and adding regression line
linreg = lm(LifeExpectancy ~ GDP)
plot(GDP, LifeExpectancy, main = "GDP vs Life Expectancy")
abline(linreg, col='red')

##Calculating Coefficient of determination
summary(linreg)
##Coefficients:
##            Estimate Std. Error t value Pr(>|t|)    
##(Intercept) 6.332e+01  8.629e-01   73.38   <2e-16 ***
##GDP         4.540e-04  4.585e-05    9.90   <2e-16 ***
##Multiple R-Squared = 0.4033

#Calculate the confidence interval
summary(linreg)
##Provides mean and error (mean = 4.540e-04 and error = 4.585e-05)
#CI = xbar + c(-ME, +ME)
#CI = (4.540e-04 - 4.585e-05), (4.540e-04 + 4.585e-05)
#CI = (0.00040815, 0.00049985)

#Hypothesis test on true slope
summary(linreg)
##Provides P value of 2e-16
##This p value is extremely small and can be rejected using even the most
##stringent threshold of .01. We can reject the null hypothesis (that it is
##equal to zero) and accept the alternative hypothesis that it is not equal to zero

##
##Compare HIV with GDP
##
##Initial scatter plot
plot(GDP, HIV, main = "GDP vs HIV")

##Calculating and adding regression line
linreg2 = lm(HIV ~ GDP)
plot(GDP, HIV, main = "GDP vs HIV")
abline(linreg2, col = 'blue')

##Calculating Coefficient of determination
summary(linreg2)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.713e+00  4.691e-01   5.783 4.33e-08 ***
#  GDP         -6.309e-05  2.493e-05  -2.531   0.0124 *  
#Multiple R-squared = 0.04231

##Calculating confidence interval
summary(linreg2)
##Provides mean and error (mean = -6.309e-05 and error = 2.493e-05)
#CI = xbar + c(-ME, +ME)
#CI = (-6.309e-05 - 2.493e-05), (-6.309e-05 + 2.493e-05)
#CI = (-8.802e-05, 0.00049985)

#Hypothesis test on true slope
summary(linreg2)
##Provides P value of 0.0124
##This p value is small but can be accepted or rejected depending on the threshold
##that you use; I will use a .05 significance level since it is the most common.
##The p value is less than the significance level and allows us to reject
##the null hypothesis that it is equal to zero and accept the alternative
##that it is not equal to zero
