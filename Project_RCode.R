#  MBA6693 - Group Project
#  Authors: Namrata Chintan Gala, Gautam Seghal, Darren Clark
#  2020-12-10

#==============================================#
#  Overview of Dataset                         #
#==============================================#

# Credit bureau data was provided which indicates the default of a number of 
# different borrowers.  Additional dmographic and other data is also provided
# to help determine what may be causing the defaults as well as what the 
# likelihood of default may be in the future for new borrowers.

#==============================================#
#  Read in Data, dplyr, ggplot, etc.           #
#==============================================#

library(ggplot2)
library(Metrics)
library(dplyr)

getwd()
credit <- read.csv("Credit_Bureau.csv", header = T)
View(credit)

#================================================#
#    Exploratory Data Analysis                   #
#================================================#

# First we will chech the data for completeness, duplicates, etc. and clean up
# as needed.




