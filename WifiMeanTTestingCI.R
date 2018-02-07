##A script to complete mean T-tests on each access point and each frequency. First, the data was split into separate vectors containing ##all data points of one access point at the same frequency. A boxplot was then created of the signal strengths for each frequency and ##access point. Mean TTests were run on each access point frequency vector in order to determine the confidence interval and wheteher ##the differences in signal strength between access points were statistically significant. The .docx file with the same name is an interpretation of these results in regard to the null and alternative hypotheses.
.
#Kyndra Zacherl
#Reading the file and attaching the columns
WifiData <- read.csv('C://Users//kzacherl//Desktop/z335_Wifi_Info_2014-09-16.csv')
attach(WifiData)

#2
#sort by AP_Name and AP_Frequency, attach column names for simple reference
splitdata <- split(WifiData,interaction(WifiData$AP_Name,WifiData$AP_Frequency))
attach(splitdata)

#3
#Show five number summary of Signal Strength for each subset and for AP_Name and AP_Frequency; if a frequency has no data points for a certain access
#point it is excluded from the list.
#Access point alsportin's iPhone caused issues due to the symbols so its name was changed to iphone in the csv.
#31 different data points of the access points with different frequencies were found.

summary(AP_Frequency)
summary(AP_Name)

summary(BBTransact.2412$AP_SignalStrength)
summary(CISACL.2412$AP_SignalStrength)
summary(CISATV.2412$AP_SignalStrength)
summary(GUGuest.2412$AP_SignalStrength)
summary(GUWireless.2412$AP_SignalStrength)
summary(ACCESS1.2422$AP_SignalStrength)
summary(iphone.2437$AP_SignalStrength)
summary(BBTransact.2437$AP_SignalStrength)
summary(CISACL.2437$AP_SignalStrength)
summary(CISATV.2437$AP_SignalStrength)
summary(GUGuest.2437$AP_SignalStrength)
summary(GUWireless.2437$AP_SignalStrength)
summary(BBTransact.2462$AP_SignalStrength)
summary(CISACL.2462$AP_SignalStrength)
summary(CISATV.2462$AP_SignalStrength)
summary(GUGuest.2462$AP_SignalStrength)
summary(GUWireless.2462$AP_SignalStrength)
summary(BBTransact.5200$AP_SignalStrength)
summary(CISACL.5200$AP_SignalStrength)
summary(GUGuest.5200$AP_SignalStrength)
summary(GUWireless.5200$AP_SignalStrength)
summary(BBTransact.5240$AP_SignalStrength)
summary(CISACL.5240$AP_SignalStrength)
summary(CISATV.5240$AP_SignalStrength)
summary(GUGuest.5240$AP_SignalStrength)
summary(GUWireless.5240$AP_SignalStrength)
summary(BBTransact.5765$AP_SignalStrength)
summary(CISACL.5765$AP_SignalStrength)
summary(CISATV.5765$AP_SignalStrength)
summary(GUGuest.5765$AP_SignalStrength)
summary(GUWireless.5765$AP_SignalStrength)

#4
#Save all data to each access point name.
ACCESS1 <- WifiData[AP_Name == 'ACCESS1',]
BBTransact <- WifiData[AP_Name == 'BBTransact',]
CISATV <- WifiData[AP_Name == 'CISATV',]
CISACL <- WifiData[AP_Name == 'CISACL',]
GUGuest <- WifiData[AP_Name == 'GUGuest',]
GUWireless <- WifiData[AP_Name == 'GUWireless',]
iphone <- WifiData[AP_Name == 'iphone',]

#Select the signal strength column and display only this data in the boxplot.
#This plot is signal strength by AP Name.
#Enter full screen mode to see all Access Point names
boxplot(ACCESS1[,5], BBTransact[,5], CISACL[,5], CISATV[,5], GUGuest[,5], GUWireless[,5], iphone[,5],
        range = 0, border = rainbow(7), xlab = "Access Point Name", ylab = "Signal Strength", names = c('ACCESS1', 'BBTransact', 'CISACL', 'CISATV', 'GUGuest', 'GUWireless', 'iphone'),
        main = "Signal Strength by Access Point Name")

#Save all data to each frequency.
Freq2412 <- WifiData[AP_Frequency == '2412',]
Freq2422 <- WifiData[AP_Frequency == '2422',]
Freq2437 <- WifiData[AP_Frequency == '2437',]
Freq2462 <- WifiData[AP_Frequency == '2462',]
Freq5200 <- WifiData[AP_Frequency == '5200',]
Freq5240 <- WifiData[AP_Frequency == '5240',]
Freq5765 <- WifiData[AP_Frequency == '5765',]

#Select the signal strength column and display only this data in the boxplot.
#This plot is signal strength by Frequency.
boxplot(Freq2412[,5], Freq2422[,5], Freq2437[,5], Freq2462[,5], Freq5200[,5], Freq5240[,5], Freq5765[,5],
        range = 0, border = rainbow(7), xlab = "Access Point Frequency", ylab = "Signal Strength", names = c('2412', '2422', '2437', '2462',
        '5200', '5240', '5765'), main = "Signal Strength by Frequency")

#5
#Confidence interval for means for each subcategory and AP_Frequency

xbar = mean(AP_Frequency)
var = var(AP_Frequency)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(AP_Frequency, conf.level=0.95)
t.test(AP_Frequency, conf.level=0.95)$conf.int

xbar = mean(BBTransact.2412$AP_SignalStrength)
var = var(BBTransact.2412$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(BBTransact.2412$AP_SignalStrength, conf.level=0.95)
t.test(BBTransact.2412$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(CISACL.2412$AP_SignalStrength)
var = var(CISACL.2412$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISACL.2412$AP_SignalStrength, conf.level=0.95)
t.test(CISACL.2412$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(CISATV.2412$AP_SignalStrength)
var = var(CISATV.2412$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISATV.2412$AP_SignalStrength, conf.level=0.95)
t.test(CISATV.2412$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(GUGuest.2412$AP_SignalStrength)
var = var(GUGuest.2412$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUGuest.2412$AP_SignalStrength, conf.level=0.95)
t.test(GUGuest.2412$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(GUWireless.2412$AP_SignalStrength)
var = var(GUWireless.2412$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUWireless.2412$AP_SignalStrength, conf.level=0.95)
t.test(GUWireless.2412$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(ACCESS1.2422$AP_SignalStrength)
var = var(ACCESS1.2422$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(ACCESS1.2422$AP_SignalStrength, conf.level=0.95)
t.test(ACCESS1.2422$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(iphone.2437$AP_SignalStrength)
var = var(iphone.2437$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(iphone.2437$AP_SignalStrength, conf.level=0.95)
t.test(iphone.2437$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(BBTransact.2437$AP_SignalStrength)
var = var(BBTransact.2437$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(BBTransact.2437$AP_SignalStrength, conf.level=0.95)
t.test(BBTransact.2437$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(CISACL.2437$AP_SignalStrength)
var = var(CISACL.2437$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISACL.2437$AP_SignalStrength, conf.level=0.95)
t.test(CISACL.2437$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(CISATV.2437$AP_SignalStrength)
var = var(CISATV.2437$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISATV.2437$AP_SignalStrength, conf.level=0.95)
t.test(CISATV.2437$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(GUGuest.2437$AP_SignalStrength)
var = var(GUGuest.2437$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUGuest.2437$AP_SignalStrength, conf.level=0.95)
t.test(GUGuest.2437$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(GUWireless.2437$AP_SignalStrength)
var = var(GUWireless.2437$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUWireless.2437$AP_SignalStrength, conf.level=0.95)
t.test(GUWireless.2437$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(BBTransact.2462$AP_SignalStrength)
var = var(BBTransact.2462$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(BBTransact.2462$AP_SignalStrength, conf.level=0.95)
t.test(BBTransact.2462$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(CISACL.2462$AP_SignalStrength)
var = var(CISACL.2462$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISACL.2462$AP_SignalStrength, conf.level=0.95)
t.test(CISACL.2462$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(CISATV.2462$AP_SignalStrength)
var = var(CISATV.2462$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISATV.2462$AP_SignalStrength, conf.level=0.95)
t.test(CISATV.2462$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(GUGuest.2462$AP_SignalStrength)
var = var(GUGuest.2462$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUGuest.2462$AP_SignalStrength, conf.level=0.95)
t.test(GUGuest.2462$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(GUWireless.2462$AP_SignalStrength)
var = var(GUWireless.2462$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUWireless.2462$AP_SignalStrength, conf.level=0.95)
t.test(GUWireless.2462$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(BBTransact.5200$AP_SignalStrength)
var = var(BBTransact.5200$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(BBTransact.5200$AP_SignalStrength, conf.level=0.95)
t.test(BBTransact.5200$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(CISACL.5200$AP_SignalStrength)
var = var(CISACL.5200$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISACL.5200$AP_SignalStrength, conf.level=0.95)
t.test(CISACL.5200$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(GUGuest.5200$AP_SignalStrength)
var = var(GUGuest.5200$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUGuest.5200$AP_SignalStrength, conf.level=0.95)
t.test(GUGuest.5200$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(GUWireless.5200$AP_SignalStrength)
var = var(GUWireless.5200$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUWireless.5200$AP_SignalStrength, conf.level=0.95)
t.test(GUWireless.5200$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(BBTransact.5240$AP_SignalStrength)
var = var(BBTransact.5240$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(BBTransact.5240$AP_SignalStrength, conf.level=0.95)
t.test(BBTransact.5240$AP_SignalStrength, conf.level=0.95)$conf.int


xbar = mean(CISACL.5240$AP_SignalStrength)
var = var(CISACL.5240$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISACL.5240$AP_SignalStrength, conf.level=0.95)
t.test(CISACL.5240$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(CISATV.5240$AP_SignalStrength)
var = var(CISATV.5240$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISATV.5240$AP_SignalStrength, conf.level=0.95)
t.test(CISATV.5240$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(GUGuest.5240$AP_SignalStrength)
var = var(GUGuest.5240$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUGuest.5240$AP_SignalStrength, conf.level=0.95)
t.test(GUGuest.5240$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(GUWireless.5240$AP_SignalStrength)
var = var(GUWireless.5240$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUWireless.5240$AP_SignalStrength, conf.level=0.95)
t.test(GUWireless.5240$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(BBTransact.5765$AP_SignalStrength)
var = var(BBTransact.5765$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(BBTransact.5765$AP_SignalStrength, conf.level=0.95)
t.test(BBTransact.5765$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(CISACL.5765$AP_SignalStrength)
var = var(CISACL.5765$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISACL.5765$AP_SignalStrength, conf.level=0.95)
t.test(CISACL.5765$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(CISATV.5765$AP_SignalStrength)
var = var(CISATV.5765$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(CISATV.5765$AP_SignalStrength, conf.level=0.95)
t.test(CISATV.5765$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(GUGuest.5765$AP_SignalStrength)
var = var(GUGuest.5765$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUGuest.5765$AP_SignalStrength, conf.level=0.95)
t.test(GUGuest.5765$AP_SignalStrength, conf.level=0.95)$conf.int

xbar = mean(GUWireless.5765$AP_SignalStrength)
var = var(GUWireless.5765$AP_SignalStrength)
t = qt(0.025, df=59, lower.tail=FALSE)
ME = t* sqrt(var/60)
xbar + c(-ME, +ME)
t.test(GUWireless.5765$AP_SignalStrength, conf.level=0.95)
t.test(GUWireless.5765$AP_SignalStrength, conf.level=0.95)$conf.int
