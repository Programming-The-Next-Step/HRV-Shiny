#RMSSD Function
rm(list=ls())

dataset <- `2019.7.10_RR_p36t0`

#HR data may be outputed from equipment as RR values or ms/time values
#let the individual specify what type of data they have

data <- as.numeric(dataset[,2])
x <- data.frame(data)




#RMSSD FUNCTION
#most common measure of heart rate variability 
myRMSSD <- function(x, input, segment, max, min) {  

  if (input == "RR" && segment == FALSE) {
    N <- length(data)
    N <- N-1
    vector <- x^2
    new <- sum(vector)/N
    rmssd1 <- sqrt(new)
    return(rmssd1)
    
  } else if (input == "MS" && segment == FALSE) {
    RR <- diff(x)
    RR <- as.numeric(RR)
    N <- length(RR)
    N <- N-1
    vector <- RR^2
    new <- sum(vector)/N
    rmssd1 <- sqrt(new)
    return(rmssd1)
    
  } else if (input == "RR" && segment == TRUE) {
    N <- length(x)
    N <- N-1
    
    #turning RR values back into milliseconds
    ###
    
    
    ########## Help please! @reviewer :)    ##########
    #I am trying to make *Vector 2* a sum of all the previous elements in *Column 1*
    #For example, if Column 1 is <- 12, 15, 10, 10, 26, 10
        #then vector 2 should be <- 12, 27, 37, 47, 73, 83
    #I feel like there is a simple answer but I can't seem to get it to work
    #thanks!! 
    
    #this doesn't work:
    sumVector <- 0
    seq1 <- nrow(x)
    seq2 <- (1:seq1)
    
    for (value in 1:length(seq2)) {
      newsum <- sum(x[1:value, ])
      sumVector <- append(sumVector, newsum)
      return(sumVector)
    }
    #######################################
    
    
    #convert millisecond to minutes so that the user can 
    #input minutes as time constraints
    #will adjust once above part is complete
    vector1 <- x
    vector2 <- vector1/1000
    newdata <- cbind(vector2, x)
    time_constrained_RRvalues <- x[min:max, 2]
    
    #back to regular rmssd
    vector <- time_constrained_RRvalues^2
    new <- sum(vector)/N
    rmssd1 <- sqrt(new)
    return(rmssd1)
    
} else if (input == "MS" && segment == TRUE) {
    RR <- diff(x)
    RR <- as.numeric(RR)
    N <- length(RR)
    N <- N-1
    #convert millisecond to seconds
    vector1 <- x
    vector1 <- vector1/1000
    newdata <- cbind(vector2, x)
    time_constrained_RRvalues <- x[min:max, 2]
    #back to regular rmssd
    vector <- RR^2
    new <- sum(vector)/N
    rmssd1 <- sqrt(new)
    return(rmssd1)
  }
}
#Example/doesn't work with RR segment data yet
new <- myRMSSD(x = data, input = "RR", segment = TRUE, min = 500, max = 15000)




#PLOT (General) HR Data 
#general plotting for a general overview of your RR data
#low RR is related to higher anxiety/distress/etc/etc
#to-do
  #time contraints later
  #x axis to minutes later
RR <- dataset[ ,2]
MS <- dataset[ ,3]
MS_to_minutes <- MS/60000
plot(RR, type = "l", xlab = "Seconds", ylab = "RR Values")

myPlotFunction <- function(input) {
  if (input == "RR") {
    plot(RR, type = "l", xlab = "Minutes", ylab = "RR Values") 
  }
  else if (input == "MS") {
    diffMS <- diff(MS)
    plot(diffMS, type = "l", xlab = "Minutes", ylab = "RR Values") 
  }
}
myPlotFunction("MS")



#SDNN VALUES
#HR data smaller than 5 minutes
#standard deviation of all RR values
#if time constraint or total is less than 5min report as SDNN
#if longer report as SDANN
SdannSdnnFunction <- function(input, time) {
if (time < 5) {
#use SDNN
  if (input == "RR") {
    sdRR <- sd(RR)
    return(sdRR)
  } else if (input == "MS") {
    diffRR <- diff(MS)
    sddiffRR <- sd(diffRR)
    return(sddiffRR)
  }
} else if (time >= 5) {
  #use SDANN -- make sure it *really* is calc the same way as SDNN
  if (input == "RR") {
    sdRR <- sd(RR)
    return(sdRR)
  } else if (input == "MS") {
    diffRR <- diff(MS)
    sddiffRR <- sd(diffRR)
    return(sddiffRR)
  }
}
}



#pNN50 Values
#tells us how many RR intervals are larger than 50 
#used more in medical (rather than psychological) research 

counter <- 0
RR2 <- RR/10
for (i in 1:length(RR2)) {
  if(RR2 > 50) {
  counter <- counter + 1
  }
}


















