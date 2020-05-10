#RMSSD Function
rm(list=ls())

dataset <- `2019.7.10_RR_p36t0`

#HR data may be outputed from equipment as RR values or ms/time values
#let the individual specify what type of data they have

#RMSSD FROM RR VALUES
myRMSSD <- function(x = dataset, input = RR) {
N <- length(dataset[ ,2])
N <- N-1
vector <- dataset[ ,2]^2
new <- sum(vector)/N
rmssd1 <- sqrt(new)
rmssd1 <- rmssd1/10
return(rmssd1)
}
rmssd1

#creating a function that uses millisecond instead of rr values 
#will eventually wrap in a function
  RR <- diff(dataset[ ,3])
  RR <- as.numeric(RR)
  N <- length(RR)
  N <- N-1
  vector <- RR^2
  new <- sum(vector)/N
  rmssd1 <- sqrt(new)
  rmssd2 <- rmssd1/10
  print(rmssd2)

##How to specify time domains 
##aka reseracher is only interested in minute 2 to minute 5
##adding a min = max = part of the function
  myRMSSD <- function(x = dataset, input = RR, min = 2, max = 5) {  
  N <- length(dataset[ ,2])
  N <- N-1
#convert millisecond to seconds
  vector1 <- dataset[ ,3]
  vector1 <- vector1/1000
  newdata <- cbind(vector2, dataset)
  time_constrained_RRvalues <- dataset[min:max, 2]
#back to regular rmssd
  vector <- time_constrained_RRvalues^2
  new <- sum(vector)/N
  rmssd1 <- sqrt(new)
  return(rmssd1)
  }
rmssd1



#Putting it all together
myRMSSD <- function(dataset = data, input = , segment = , min = , max = ) {  
  if (input == "RR" && segment = FALSE) {
    N <- length(dataset[ ,2])
    N <- N-1
    vector <- dataset[ ,2]^2
    new <- sum(vector)/N
    rmssd1 <- sqrt(new)
    print(rmssd1)
  } else if (input == "MS" && segment == FALSE) {
    RR <- diff(dataset[ ,3])
    RR <- as.numeric(RR)
    N <- length(RR)
    N <- N-1
    vector <- RR^2
    new <- sum(vector)/N
    rmssd1 <- sqrt(new)
    print(rmssd1)
  } else if (input == "RR" && segment == TRUE) {
    N <- length(dataset[ ,2])
    N <- N-1
    #convert millisecond to seconds
    vector1 <- dataset[ ,3]
    vector2 <- vector1/1000
    newdata <- cbind(vector2, dataset)
    time_constrained_RRvalues <- dataset[min:max, 2]
    #back to regular rmssd
    vector <- time_constrained_RRvalues^2
    new <- sum(vector)/N
    rmssd1 <- sqrt(new)
    print(rmssd1)
  } else if (input == "MS" && segment == TRUE) {
    RR <- diff(dataset[ ,3])
    RR <- as.numeric(RR)
    N <- length(RR)
    N <- N-1
    #convert millisecond to seconds
    vector1 <- dataset[ ,3]
    vector1 <- vector1/1000
    newdata <- cbind(vector2, dataset)
    time_constrained_RRvalues <- dataset[min:max, 2]
    #back to regular rmssd
    vector <- RR^2
    new <- sum(vector)/N
    rmssd1 <- sqrt(new)
    print(rmssd1)
  }
}
  






