#-----------------------------------------------------------------------------------
# Load Clean Data File
#-----------------------------------------------------------------------------------

setwd("~/Documents/Rajendra Chattergoon/CU Boulder/Consulting/Stephanie Chasteen/2015-16 NFW/1516 NFW Survey")
source("Raw_Data.R")

#-----------------------------------------------------------------------------------
# Analysis - Section 3
#-----------------------------------------------------------------------------------

library(psych)
library(car)
library(sjPlot)

describe(NFW[,45:60])
describe(NFW[,62:89])
describe(NFW[,91:94])

NFW[is.na(NFW)] <- 0

NFW$Q11a <- NFW$X45 + NFW$X46 + NFW$X47
NFW$Q11b <- NFW$X49 + NFW$X50 + NFW$X51
NFW$Q11c <- NFW$X53 + NFW$X54 + NFW$X55
NFW$Q11d <- NFW$X57 + NFW$X58 + NFW$X59
NFW$Q12a <- NFW$X62 + NFW$X63 + NFW$X64
NFW$Q12b <- NFW$X66 + NFW$X67 + NFW$X68
NFW$Q12c <- NFW$X70 + NFW$X71 + NFW$X72
NFW$Q12d <- NFW$X74 + NFW$X75 + NFW$X76
NFW$Q12e <- NFW$X78 + NFW$X79 + NFW$X80
NFW$Q12f <- NFW$X82 + NFW$X83 + NFW$X84
NFW$Q12g <- NFW$X86 + NFW$X87 + NFW$X88 
NFW$Q13a <- NFW$X91 + NFW$X92 + NFW$X93 

describe(NFW[,133:144])

