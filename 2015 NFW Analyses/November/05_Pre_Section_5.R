#-----------------------------------------------------------------------------------
# Load Clean Data File
#-----------------------------------------------------------------------------------

setwd("~/Documents/Rajendra Chattergoon/CU Boulder/Consulting/Stephanie Chasteen/2015-16 NFW/1516 NFW Survey")
source("Raw_Data.R")

#-----------------------------------------------------------------------------------
# Analysis - Section 8
#-----------------------------------------------------------------------------------

library(psych)

table(NFW$X122); table(NFW$X122)/sum(table(NFW$X122))

describe(NFW[,123:128])

table(NFW$X129); table(NFW$X129)/sum(table(NFW$X129))
table(NFW$X130); table(NFW$X130)/sum(table(NFW$X130))

table(NFW$X131); table(NFW$X131)/sum(table(NFW$X131))
table(NFW$X132); table(NFW$X132)/sum(table(NFW$X132))
