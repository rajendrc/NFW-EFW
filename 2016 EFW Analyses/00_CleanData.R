#---------------------------------------------------------------------------
# 00. Import and Clean the Raw Data.
#---------------------------------------------------------------------------

library(readxl)
library(psych)
library(ltm)

# Set the working directory to the location of the raw excel file. 
setwd("~/Dropbox/NFW-EFW Analyses/Raw Data/March 2016")

# Import the raw data files. 
raw.pre <- read_excel("Actual-responses-EFW.xls", sheet=1, col_names=FALSE, skip=2)
raw.post <- read_excel("EFW-Post-numerical-response.xls", sheet=1, col_names=FALSE, skip=2)

# Rename the merge variable 
colnames(raw.pre)[9] <- "Email"
colnames(raw.post)[9] <- "Email"

# Rename emails of identical individuals with different emails in pre and post
# Use pre-survey email address
raw.post[raw.post$X10=="Worchesky",9] <- "Dr.Worchesky@umbc.edu"
raw.post[raw.post$X10=="Abdul-Razzaq",9] <- "wabdulra@wvu.edu"
raw.post[raw.post$X10=="Hicks",9] <- "hicks@udallas.edu"

# Remove respondents that were not registered for the conference.
EFW.pre <- raw.pre[raw.pre$X1!=0,]
EFW.post <- raw.post
  
# Merge respondent answers from the post-survey
EFW.pp <- merge(raw.pre, raw.post, by="Email")

rm(raw.pre, raw.post)
