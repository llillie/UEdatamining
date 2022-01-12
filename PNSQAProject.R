library(ggplot2)
library(dplyr)
library(readr)

setwd(dir="C:/Users/alillie/Documents/RSQAData/PNSQAStart")

PNSQASdata <- read.csv("Results.csv")
View(PNSQASdata)

setwd(dir="C:/Users/alillie/Documents/RSQAData/PRISMData/PRISM_PointData_RSQA_PPT_Temp")

PNSQASPRISMdata <- read.csv("PRISM_ppt_tmean_stable_4km_201501_201509_PNSQA.csv")
View(PNSQASPRISMdata)

#In theory, I think this should work, but it's not working. When I run this code, I am returned with a single column
#of all the variables and I don't know how to change that.

setwd(dir="C:/Users/alillie/Documents/RSQAData/PNSQAStart")

PNSQASdata <- read.csv("Results.csv")
View(PNSQASdata)