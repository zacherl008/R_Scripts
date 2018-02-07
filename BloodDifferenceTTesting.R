##A script that runs on blood cholestrol level/age/BMI data. The data is plotted by both boxplots, scatter plots, and histograms.
##Any missing data is replacing with na via a loop for each column. A T test was run in order to determine whether blood cholestrol
##levels have a statistically significant difference for each BMI and these results were interpreted in terms of the P value and the 
##null and alternative hypotheses.

##Kyndra Zacherl
##Reading CCD data
data <- read.table('C://Users/kzacherl//Desktop/Rdata/CCDdata.txt', header = TRUE)

##Five number summaries
summary(data$age)
summary(data$BMI)
summary(data$HbA1c)
summary(data$HbA1cbaseline)
summary(data$CRP)
summary(data$CRPbaseline)

#Boxplots
boxplot(data$age)
boxplot(data$BMI)
boxplot(data$HbA1c)
boxplot(data$HbA1cbaseline)
boxplot(data$CRP)
boxplot(data$CRPbaseline)

#Replace missing data
for(i in 1:length(data$age)){
  if(data$age[i] == 999){
    data$age[i] <- NA
  }
}

for(i in 1:length(data$BMI)){
  if(data$BMI[i] == 999.0){
    data$BMI[i] <- NA
  }
}
  
for(i in 1:length(data$HbA1c)){
  if(data$HbA1c[i] == 999.000){
    data$HbA1c[i] <- NA
  }
}

for(i in 1:length(data$CRP)){
  if(data$CRP[i] == 999.00){
    data$CRP[i] <- NA
  }
}

for(i in 1:length(data$CRPbaseline)){
  if(data$CRPbaseline[i] == 999.000){
    data$CRPbaseline[i] <- NA
  }
}

##Histograms
hist(data$age)
hist(data$BMI)
hist(data$HbA1c)
hist(data$HbA1cbaseline)
hist(data$CRP)
hist(data$CRPbaseline)

##difference 
HbA1cDiff = data$HbA1c - data$HbA1cbaseline
hist(HbA1cDiff)

CRPDiff = log(data$CRP) - log(data$CRPbaseline)
hist(CRPDiff)

##scatter plot
linreg = lm(data$BMI~HbA1cDiff)
plot(HbA1cDiff, data$BMI, main = "HbA1cDiff vs BMI")
abline(linreg, col='red')

linreg2 = lm(data$BMI~CRPDiff)
plot(CRPDiff, data$BMI, main = "CRPDiff  vs BMI")
abline(linreg, col='blue')

##TTest
attach(split(data, diet))

#L is for low diet, H is for high
lowDifference = L$HbA1cbaseline - L$HbA1c
t.test(lowDifference, mu = 0, alternative ='two.sided')
##The P value provided from this t test is 1.74 * 10^-6. This is an extremely low
##p value and even using the lowest significance value of .001 leads to the 
##conclusion of strongly rejecting the null hypothesis that the mean is 0 and 
##accepting the alternative hypothesis that it is not zero.
##The confidence interval is (-0.006985781, -0.003218301)

highDifference = H$HbA1cbaseline - H$HbA1c
t.test(highDifference, mu = 0, alternative ='two.sided')
##The P value provided from this t test is 5.22 * 10^-5. This is an extremely low
##p value and even using the lowest significance value of .001 leads to the 
##conclusion of strongly rejecting the null hypothesis that the mean is 0 and 
##accepting the alternative hypothesis that it is not zero.
##The confidence interval is (-0.005537663, -0.002120873)

lowDifference2 = log(L$CRPbaseline) - log(L$CRP)
t.test(lowDifference2, mu = 0, alternative ='two.sided')
##The P value provided from this t test is 0.009833. This is a low
##p value and using a significance value of .01 leads to the 
##conclusion of rejecting the null hypothesis that the mean is 0 and 
##accepting the alternative hypothesis that it is not zero.However, if a 
##significance level of .001 was used like above, the p value would not be
##under the significance level. This conclusion is not as strong as the two above.
##The confidence interval is (0.07724854, 0.53536974)

highDifference2 = log(H$CRPbaseline) - log(H$CRP)
t.test(highDifference2, mu = 0, alternative ='two.sided')
##The P value provided from this t test is 0.7866. This is a very high p value
##and is not under even the highest significance level of 0.10.
##The conclusion here is that there is not enough evidence to reject the
##null hypothesis.
##The confidence interval is (-0.1883411, 0.1435815).
