#' Time Domain Heart Rate Variability Analysis

#' Heart rate variability uses the fluctuation in the ammount of time that occurs between
#' heart beats. Time Domain Heart Rate Variability Analysis is one way to analyze and interpret
#' heart rate data. Time domain indicies quantify the amount of variance in the
#' inter-beat-intervals (IBI). It is frequently used across different domains of
#' research, such as medicine, human kinetcs, and psychology. Benefits of this method
#' is that the cost of collecting this type of data is relatively low compared to other
#' methods and statistical outputs can be compared across individuals.


#' Part One: Data Organization

#' Input Data Organization/Cleaning Function
#' This function takes the user's heart rate variation (HRV) data and turns it into a
#' useable form for the rest of this package. Output from heart rate monitors used in research
#' are typically either in RR values or MS values. RR values are the milliseconds ellapsed
#' between successive heart beats, while MS values are a chronological millisecond value that
#' marks when each heart beat occurs. All statistical values used in HRV take RR values
#' as an input. Therefore, if the user chooses to upload MS values, the function will
#' transform it into RR values.
#'
#' Additionally, the function allows the user to specify the time-frame that they
#' are interested in, using the "segment" option. The function intakes a minute min/max
#' value (i.e., if the user is only interested in that data that occured between
#' minute 2 and minute 7.5, they would input those values as the min/max).
#'
#' In the literature, the HRV segments less than 30 seconds are not valid and their
#' use is not advised. Therefor, the program writes a warning if the user chooses to
#' to use a HRV segment less than 30 seconds.
#'
#' The function assumes that the user is uploading a csv file with the ms or RR values
#' in a single row. This matches the type of output given by most heart rate monitoring
#' software.
#'
#' @param infile Path to the input file
#' @return
#' @export

Data_Input_Function <- function (imported_data, input, segment, min, max) {

  if (input == "RR" && segment == FALSE) {

    x <- as.data.frame(as.numeric(imported_data[ ,1]))

    # creating a vector that transforms RR values to milliseconds

    time_vector <- vector()

    for (value in 1:nrow(x)) {

      newsum <- sum(x[1:value, ])

      time_vector <- append(time_vector, newsum)
    }

    # transforming millisecond values to minutes

    minutes_vector <- time_vector / 60000

    # then add this to existing dataframe, so that the user can index RR values by time

    data <- cbind(minutes_vector, x)

  } else if (input == "MS" && segment == FALSE) {

    # diff function that takes the differences between each millisecond time point

    # turns the data into RR values

    x <- as.data.frame(diff(imported_data[ ,1]))

    # creating a vector that transforms RR values to milliseconds

    time_vector <- vector()

    for (value in 1:nrow(x)) {

      newsum <- sum(x[1:value, ])

      time_vector <- append(time_vector, newsum)
    }

    # millisecond to minutes

    minutes_vector <- time_vector / 60000

    # then add this to existing dataframe, so that the user can index time

    data <- cbind(minutes_vector, x)

  } else if (input == "RR" && segment == TRUE) {

    x <- as.data.frame(imported_data[ ,1])

    # create a new vector that converts RR values to time

    time_vector <- vector()

    for (value in 1:nrow(x)) {

      newsum <- sum(x[1:value, ])

      time_vector <- append(time_vector, newsum)
    }

    # millisecond to minutes

    minutes_vector <- time_vector / 60000

    # then add this to existing dataframe, so that the user can index time

    df1_plus_time <- cbind(minutes_vector, x)

    # referencing the row number based on the minimum value that the user inputs

    min_row <- which(grepl(min, df1_plus_time[ ,2]))

    min_row <- min_row[1]

    # referencing the row number based on the maximum value that the user inputs

    max_row <- which(grepl(max, df1_plus_time[ ,2]))

    max_row <- tail(max_row)

    max_row <- max_row[6]

    # narrowing down the data frame so that it only includes the RR values

    # within the user's specified time frame

    data <- df1_plus_time[min_row:max_row, 1:2]

  } else if (input == "MS" && segment == TRUE) {

    x <- as.data.frame(diff(imported_data[ ,1]))

    time_vector <- vector()

    for (value in 1:nrow(x)) {

      newsum <- sum(x[1:value, ])

      time_vector <- append(time_vector, newsum)

    }


    # millisecond to minutes

    minutes_vector <- time_vector / 60000

    # then add this to existing dataframe, so that the user can index time

    df1_plus_time <- cbind(minutes_vector, x)

    # referencing the row number based on the minimum value that the user inputs

    min_row <- which(grepl(min, df1_plus_time[ ,2]))

    min_row <- min_row[1]

    # referencing the row number based on the maximum value that the user inputs

    max_row <- which(grepl(max, df1_plus_time[ ,2]))

    max_row <- tail(max_row)

    max_row <- max_row[6]

    # narrowing down the data frame so that it only includes the RR values

    # within the user's specified time frame

    data <- df1_plus_time[min_row:max_row, 1:2]

  }


  # warning messages for incorrect data input

  last_minute <- tail(data[,1])

  last_minute <- last_minute[0]

  if (last_minute < .5) {

    warning("This software is not appropriate for heart rate
            segments smaller than 30 seconds.")

  }

  if (segment = TRUE) {

    if (min > max) {

      warning("Incorrect min/max values. Max value should exceed min value by
              at least 30 seconds.")

    }

    range <- max - min

    if (max - min < 0.5) {

      warning("This software is not appropriate for heart rate
            segments smaller than 30 seconds.")

    }

  }


  return(data)

}



#' Part Two: Time-Domain Statistical Methods

#' Root Mean Squared Successive Differences (RMSSD)

#' RMSSD is calculated using the successive differences ellapsed between heart
#' beats (RR values). This is considered the most precise marker for parasympathetic
#' activity using heart rate, and it is the most common time-domain HRV statistic reported
#' in the literature.
#'
#' RMSSD values can be subject to individul differences, so it is best to compare RMSSD
#' values between groups or across time, rather than a single value indicating a validated
#' level of distress.
#'
#' Lower HRV and RMSSD values indicate higher levels of distress.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export

RMSSD_Calculator  <- function(data = data) {

    input_length <- length(data[,2]) - 1

    RR_squared <- data^2

    RR_squared_average <- sum(RR_squared) / input_length

    rmssd1 <- sqrt(RR_squared_average)

    return(rmssd1)

}



#' SDNN Function

#' SDNN measurement is the standard deviation of all time ellapses heart beats.
#' This value indicates both parasympathetic and sympathetic nervous system
#' activity and is imporant measure in cardiology research and practice
#' It is not often used in psychological research and is most often used in medical research
#' when heart rate recordings are more than 24 hours. Values below 50ms indicate risk,
#' while values above 100ms are relativily healthy
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export


SDNN_Calculation <- function(data) {

  last_minute <- tail(data[,1])

  last_minute <- last_minute[5]

  if (last_minute < 1440) {

    warning("SDNN is most accurate with heart rate segments longer than 24 hours.")

  }

  SDNN <- sd(data[,2])

  return(SDNN)

}


#' NN50 Calculation

#' NN50 number of intervals between heart beats that are greater than 50ms.
#' This measure is used more in medical research and practice than psychological research.
#' This is more useful when calculated as a percentage (next function).
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export


NN50_Count_Function <- function(data = data) {

  RR <- data[ ,2]

  counter <- 0

  for (i in 1:length(RR)) {

    if (i > 50) {

      counter <- counter + 1

    }

  }

  return(counter)

}


#' pNN50 Calculation

#' This value is the percentage of intervals between heart beats that are
#' greater than 50ms when compared to all heart beats in the segment.
#'
#' Some researchers suggest that less than 6.25% of heart beats greater than 50ms
#' indicates an above normal heart rate, putting the individual at cardiac risk or
#' indicating high levels of distress.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export

pNN50_Percentage_Function <- function (data) {

  RR <- data[,2]

  counter <- 0

  for (i in 1:length(RR)) {

    if (i > 50) {

    counter <- counter + 1

  }

  RR_length <- length(RR)

  pNN50_Percent <- counter / RR_length

  return(pNN50_Percent)

  }

}


#' Descriptive Heart Rate -- MIN + MAX + AVERAGE

#' Gives the user an overview of the data.
#'
#' HR range (HR max - HR min) is sensitive to Respiratory Sinus Arrhythmia (RSA),
#' and higher HR range reflects the hearts ability to adapt to the environment.
#' Lower HR range, in addition to other measures, can be used to diagnose cardiac
#' risk or as an indicator of distress. It is not advised to use this measurement
#' if the HRV segment is less than 2-minutes.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export

Descriptive_HRV_Function <- function (data = data) {

  last_minute <- tail(data[,1])

  last_minute <- last_minute[5]

  if (last_minute < 2) {

    warning("It is not advised to use HR range with segments smaller than 2 minutes.")

  }

  RR <- data[ , 2]

  min_max_difference <- max(RR) - min(RR)

  return <- (list(min_max_difference, mean(RR), min(RR), max(RR)))

  return(return)

}


#' Part Three: Extra Functions

#' Plot Function

#' Gives the user an overview of the data.
#' Time on the x axis, RR value on the y axis.
#' Lower RR values indicate a faster heart rate.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export

RR_plot_function <- function (data = data) {

  plot(data, type = "p", xlab = "Minutes", ylab = "RR Values", col=rgb(0.4,0.4,0.8,0.6), pch=16 , cex=.5)

  lines(predict(loess(time ~ RR, data)), col='red', lwd=1)

}


# Turning this into a package
# https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
# install.packages("devtools")
# install.packages("roxygen2")
library("devtools")
library("roxygen2")
#devtools::create("TimeDomainAnalysis")
devtools::document()
usethat::use_vignette("Introduction")

#how to install this package (not currently ready)
#devtools::install_github("yourusername/TimeDomainAnalysis")




