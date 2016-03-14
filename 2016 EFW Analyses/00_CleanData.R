#---------------------------------------------------------------------------
# 00. Import and Clean the Raw Data.
#---------------------------------------------------------------------------

library(readxl)
library(psych)
library(ltm)

# Set the working directory to the location of the raw excel file. 
setwd("~/Dropbox/EFW Analyses/Raw Data/March 2016")

# Import the raw data file. 
raw <- read_excel("Actual-responses-EFW.xls", sheet=1, col_names=FALSE, skip=2)

# Remove respondents that were not registered for the conference.
EFW <- raw[raw$X1!=0,]

