setwd('Z:/RData')

skeletons = read.table('SkeletonData2.txt', header=T)

attach(skeletons)

linreg = lm(DGDifference~BMIquant)

e2=var(linreg$residuals)*400

x2=var(BMIquant)*400

se=sqrt(x1)/sqrt(398*x2)

pt(0.4061/se, df=398, lower.tail=F)

summary(linreg)