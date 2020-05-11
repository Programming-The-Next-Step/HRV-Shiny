# HRV-Shiny
A shiny app for time-domain heart rate variation analysis -- useful for experiments that record heart rate.
<<<<<<< Updated upstream
Project Overview

Goal: To create a shiny app that completes time-domain heart rate variation analysis for the user. 

Background
•	“Heart rate variation/variability” (HRV) refers to the elapse of time between heartbeats. 
•	HRV has been implemented as a measure to study sleep, physical recovery, and overall health. This has also been used as a measure of stress or anxiety in psychological studies. Specifically, most research has found that higher anxiety leads to lower HRV.

Time-Domain Analysis  
•	A way of quantifying the amount of variability between heartbeats. This is used when the equipment records when each heartbeat occurs in milliseconds. 

Important Values / What the program will calculate for the user.
SDNN
•	Basically, SDNN is the standard deviation when distributing all of the millisecond differences between heartbeats
•	This measurement is related strongly to SNS and PNS activity
SDANN
•	like the SDNN but a way of indicating that the data is broken into/analyzed in smaller intervals 
•	For example, if you were interested examining the HRV differences between 2+ parts of your experiment
SDANN Index
•	combining all of the SDANN 5-minute intervals into 1 and comparing the differences between intervals 
RMSSD
•	the root mean square* of successive differences between normal heartbeats
•	obtained by calculating each successive time difference between heartbeats in ms
•	then, each of the values is squared and the result is averaged
•	What does this show us? A more comprehensive value for heart rate variation. 
•	This value is often reported as HRV. 
•	Lower RMSSD is related to “lower HRV” which has been found in previous anxiety/phobia literature.
HR Max – HR MIN
•	Plot these two against each other for another measure of variation
•	You need at least a 2.5 minute sample for it to be valid.
NN50
•	looking at all of time times beat intervals were more than 50ms apart
•	more important for medical research – a way to find/diagnose heart arrhythmia 

Non-linear measurements
Poincaré Plot
•	graphed by plotting every R-R interval against the prior interval, creating a scatter plot
•	this allows researchers to visually search for differences within a time series
•	sensitive to changes in R-R intervals
•	we can analyze the Poincaré plot by fitting an ellipse to the plot
•	from this we can derive three non-linear measurements S, SD1 and SD2
•	SD1 – standard deviation of the distance of each point from the y = x axis, short term HRV in ms and correlated to baroreflex sensitivity* – very close to RMSSD. 
•	SD2 – standard deviation of each point from the y = x + average R-R interval. Specifies the length of the ellipse in the plot. Measures both long and short term HRV. 
•	The ratio between SD1 and SD2 can tell us the unpredictability of the HRV in the time series. 
•	Still figuring out why this is important/offers more information than just using time-domain method.
•	Could be eliminated if there isn’t enough time.


Extra Tasks 
Period Length – you can do a power calculation, but most literature suggests a minimum of 2.5 minutes.
•	If there is extra time, I would include a power calculator in the app, if not, I will just warn the user against using a time segment smaller than 2.5 minutes.

•	Data Cleaning – if there is time I might make a function that removes outliers for the user. If there isn’t time I would advise the user to clean their own data. 
=======



