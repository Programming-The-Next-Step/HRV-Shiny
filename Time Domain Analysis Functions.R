# Time-Domain Heart Rate Variability Analysis Shiny App 

# http://adv-r.had.co.nz/Style.html

rm(list=ls())

dataset <- `2019.7.10_RR_p36t0`

data <- as.numeric(dataset[,2])
x <- data.frame(data)

# Program uses RR rather than NN values
# RR includes all heart beats while NN is for only "normal" heart beats

# RMSSD FUNCTION
# most common measure of heart rate variability 

RMSSD_Calculator  <- function(x, input, segment, max, min) {  

  if (input == "RR" && segment == FALSE) {
    
    input_length <- length(data)
    
    input_length <- input_length - 1
    
    RR_squared <- x^2
    
    RR_squared_average <- sum(RR_squared) / input_length
    
    rmssd1 <- sqrt(RR_squared_average)
    
    return(rmssd1)
    
  } else if (input == "MS" && segment == FALSE) {
    
    RR <- diff(x)
    
    RR <- as.numeric(RR)
    
    input_length <- length(data)
    
    input_length <- input_length - 1
    
    RR_squared <- x^2
    
    RR_squared_average <- sum(RR_squared) / input_length
    
    rmssd1 <- sqrt(RR_squared_average)
    
    return(rmssd1)
    
  } else if (input == "RR" && segment == TRUE) {
    
    input_length <- length(data)
    
    input_length <- input_length - 1
    
    # create a new vector that converts RR values to time 
   
    time_vector <- vector()
    
    for (value in 1:nrow(x)) {
    
        newsum <- sum(x[1:value, ])
      
        time_vector <- append(sumVector, newsum)
    }

    # millisecond to minutes
    
    df1 <- time_vector / 1000
    
    # then add this to existing dataframe, so that the user can index time
    
    df1_plus_time <- cbind(df1, x)
    
    time_constrained_RRvalues <- df1_plus_time[min:max, 2]
    
    # back to regular rmssd
    
    RR_squared <- time_constrained_RRvalues ^ 2
    
    RR_squared_average <- sum(RR_squared) / input_length
    
    rmssd1 <- sqrt(RR_squared_average)
    
    return(rmssd1)
    
  } else if (input == "MS" && segment == TRUE) {
    
    RR <- diff(x)
  
    RR <- as.numeric(RR)
  
    input_length <- length(data)
  
    input_length <- input_length - 1
  
    # convert MS values to seconds
  
    df1 <- x / 1000
    
    newdata <- cbind(df1, x)
    
    time_constrained_RRvalues <- newdata[min:max, 2]
    
    #back to regular rmssd
    
    RR_squared <- time_constrained_RRvalues ^ 2
    
    RR_squared_average <- sum(RR_squared) / input_length
    
    rmssd1 <- sqrt(RR_squared_average)
    
    return(rmssd1)
  
    }

  }


# Plotting RR values data for a general overview 

RR_plot_function <- function (x = data, input =, segment = , min = , max =) {
  
  if (input = "RR" && segment = FALSE) {
  
    RR <- data[ , 1]  
    
    plot(RR, type = "l", xlab = "Seconds", ylab = "RR Values")
  
    }
  
  else if (input = "MS" && segment = FALSE) {
    
    RR <- diff(data [ , 1]) 
    
    plot(RR, type = "l", xlab = "Seconds", ylab = "RR Values")
    
  }
  
  else if (input = "RR" && segment = TRUE) {
  
    # segment allows the individual to only view a plot or data within a certain time frame  
    
    RR <- data[ , 1]
    
    #create an extra column of milliseconds by converting the RR values 
    
    time_vector <- vector()
    
    for (value in 1:nrow(x)) {
      
      newsum <- sum(x[1:value, ])
      
      time_vector <- append(sumVector, newsum)
      
    }
    
    seconds_vector <- time_vector / 1000
    
    newdata <- cbind(seconds_vector, RR)
    
    time_constrained_RRvalues <- newdata[min:max, 2]
    
    plot(time_constrained_RRvalues, type = "l", xlab = "Seconds", ylab = "RR Values")
    
  }
  
  else if (input = "MS" && segment = TRUE) {
    
    RR <- diff(data[ , 1])
    
    #create an extra column of milliseconds by converting the RR values 
    
    time_vector <- vector()
    
    for (value in 1:nrow(x)) {
      
      newsum <- sum(x[1:value, ])
      
      time_vector <- append(sumVector, newsum)
      
    }
    
    seconds_vector <- time_vector / 1000
    
    newdata <- cbind(seconds_vector, RR)
    
    time_constrained_RRvalues <- newdata[min:max, 2]
    
    plot(time_constrained_RRvalues, type = "l", xlab = "Seconds", ylab = "RR Values")
    
  }

}


# SDRR vs. SDARR Calculation Function 

# SDRR and SDARR are just a way to discirminate between a segment shorter 

# or longer than 5 min -- because segments longer than 5min are more reliable

# both are the standard deviation of all RR values


SdannSdnnFunction <- function(input) {
  
if (input = "RR") {

  RR <- data[ , 1]
  
  time_vector <- vector()
  
  for (value in 1:nrow(x)) {
    
    newsum <- sum(x[1:value, ])
    
    time_vector <- append(sumVector, newsum)
    
  }
  
} else if (input = "MS") {
  
  time_vector <- data[ ,1]
  
}
  
  minutes <- time_vector / 60000
  
# then goes into determining if it is an SDRR or SDARR
  
  if (tail(minutes) < 5) {
    
    # use SDNN
    
    if (input == "RR") {
    
      sdRR <- sd(data[,1])
    
      return(sdRR)
  
    } else if (input == "MS") {
    
      diffRR <- diff(data[,1])
    
      sddiffRR <- sd(diffRR)
    
      return(sddiffRR)
      
    }

  } else if (tail(minutes) >= 5) {
    
    # use SDARR
  
      if (input == "RR") {
    
        sdRR <- sd(data[,1])
    
        return(sdRR)
  
    } else if (input == "MS") {
    
        diffRR <- diff(data[,1])
    
        sddiffRR <- sd(diffRR)
    
        return(sddiffRR)
  
        }

    }

  }


# pNN50 Values

# tells us how many RR intervals are larger than 50 

# used more in medical rather than psychological research 


pNN50_Function <- function(x, input) {

  if (input = "RR") {

    RR <- data[ , 1]

  }

  if (input = "MS") {
    
    RR <- diff(data[ , 1])
    
  }
  
  counter <- 0
  
  for (i in 1:length(RR)) {
    
    if(RR > 50) {
      
      counter <- counter + 1
      
    }
  
  }
  
}




# Descriptive Heart Rate -- MIN + MAX + AVERAGE

Descriptive_HR_Function <- function (data, input) {
  
  if (input = "MS") {
    
    RR <- diff(data [ ,1])
    
  }
  
  else if (input = "RR") {
    
    RR <- data[ ,1]
    
  }
  
  return(mean(RR))
  
  return(min(RR))
  
  return(max(RR))
  
}

# Time Point Analysis 

# Not part of the time-domain analysis protocol, but sometimes useful for researchers if they are interested

# in comaring heart rate at a baseline vs. at a particular time-point in the experiment 

Time_Point_Analysis_Function <- function (data, input, time1, time2, time3, time4) {
  
  if (input = "MS") {
    
    RR <- diff(data [ ,1])
    
  }
  
  else if (input = "RR") {
    
    RR <- data[ ,1]
    
  }
  
  # make a time variable to be able to referece RR values
  
  for (value in 1:nrow(data)) {
    
    newsum <- sum(data[1:value, ])
    
    time_vector <- append(sumVector, newsum)
    
  }
  
  seconds_vector <- time_vector / 1000
  
  newdata <- cbind(seconds_vector, RR)
  
  if (time1 = TRUE) {
    
    time1 <- newdata[time1, ]
    
  }
  
  if (time2 = TRUE) {
    
    time2 <- newdata[time2, ]
    
  }
  
  if (time3 = TRUE) {
    
    time3 <- newdata[time3, ]
    
  }
  
  if (time4 = TRUE) {
    
    time4 <- newdata[time4, ]
    
  }
  
}




# SDARR Index 

# this is where the user can decide if they want multiple analysis completed

# on different segments of the data

# i.e., they have a 8 minute segment, and they want RMSSD values for 

# minute 2, 3, 4, 5


Segmenting_Function <- function(data, segment1, segment2, segment3, segment4, segment5) {

  
}















