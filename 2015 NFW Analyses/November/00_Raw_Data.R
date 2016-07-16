# Import and Merge Raw Data Files

library(readxl)
setwd("~/Documents/Rajendra Chattergoon/CU Boulder/Consulting/Stephanie Chasteen/2015-16 NFW/1516 NFW Survey")

#-----------------------------------------------------------------------------------
# Pre-Survey Responses
#-----------------------------------------------------------------------------------

# NFW <- read_excel("2015_1023_NFW_Survey.xls",
#                      sheet=1,
#                      col_names=FALSE)
# NFW <- NFW[3:67,] # update lower bound with number of observations
# cols = c(1, 14:21, 23:32, 34:43, 45:60, 62:89, 91:94, 96:132);    
# NFW[ ,cols] = apply(NFW[, cols], 2, 
#                    function(x) as.numeric(x))
# rm(cols)

#-----------------------------------------------------------------------------------
# Post-Survey Responses
#-----------------------------------------------------------------------------------

NFW <- read_excel("2015_1215_NFW_Survey.xls",
                      sheet=1,
                      col_names=FALSE)
NFW <- NFW[3:64,] # update lower bound with number of observations
cols = c(1, 14:28, 30:50, 52:55, 60:65);    
NFW[ ,cols] = apply(NFW[, cols], 2, 
                    function(x) as.numeric(x))
NFW <- NFW[ ,cols]
rm(cols)
