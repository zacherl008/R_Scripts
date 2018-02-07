##A function designed to compare two different wifi access points broadcasting the same frequency and match them based on identical 
##fingerprints and time stamps. The signal strength at each matching point in time are retrieved and compared. A vector is then returned
##of the differences between times with a matching fingerprint in both access points. A hypothesis test is then run on the values
##contained in this vector to determine whether the difference in signal strength between access points is significant.

##Kyndra Zacherl
##Reading WifiData csv
WifiData <- read.csv('C://Users//kzacherl//Desktop/z335_Wifi_Info_2014-09-16.csv')
attach(WifiData)
##Writing function
APDifference <- function(AP1, AP2, frequency){
  ##Declaring vector
  vec <- vector()
  ##Generating data sets using user entered arguments
  dataset1 <- WifiData[ which(WifiData$AP_Name == AP1 & WifiData$AP_Frequency == frequency),]
  dataset2 <- WifiData[ which(WifiData$AP_Name == AP2 & WifiData$AP_Frequency == frequency),]
  ##Determining length of smallest data set for for loop
  if((nrow(dataset1)) > (nrow(dataset2))){
    length <- nrow(dataset2) 
  }
  else{
    length <- nrow(dataset1)
  }
  if ((nrow(dataset1) == 0) || (nrow(dataset2) == 0)){
    print("At least one data set contains no data. Cannot run function.")
  } 
  ##Loop to check matching fingerprints and timestamps
  for(i in 1:length){
      if(dataset1$Fingerprint_ID[i] %in% dataset2$Fingerprint_ID){
        FP = dataset1$Fingerprint_ID[i]
        RN = which(dataset2$Fingerprint_ID == FP)
        if(dataset1$TimeStamp[i] == dataset2$TimeStamp[RN]){
          ##Finding Signal Strength for both AP 
          SS1 = dataset1$AP_SignalStrength[i]
          SS2 = dataset2$AP_SignalStrength[RN]
          ##Calculating Difference
          SSDifference = SS1 - SS2
          ##Adding difference to vector
          vec[i] <- SSDifference
        }
      }
  }
  ##Returning vector
  return(vec)
}
##Saving output of function for statistical testing
vector <- APDifference('CISATV', 'CISACL', '2412')

##Hypothesis test for mean
t.test(vector, mu = 0, alternative = 'two.sided')

vector = abs(vector)
sum = sum(vector)
length = length(vector)
distance =  (sum) / (length)
