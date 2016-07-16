#-----------------------------------------------------------------------------------
# Load Clean Data File
#-----------------------------------------------------------------------------------

setwd("~/Documents/Rajendra Chattergoon/CU Boulder/Consulting/Stephanie Chasteen/2015-16 NFW/1516 NFW Survey")
source("Raw_Data.R")

#-----------------------------------------------------------------------------------
# Analysis - Section 1
#-----------------------------------------------------------------------------------

library(psych)
library(ltm)
# library(sjPlot)


X15<-table(NFW$X15); table(NFW$X15)/sum(table(NFW$X15))
X16<-table(NFW$X16); table(NFW$X16)/sum(table(NFW$X16))

barplot(X15, ylab="Frequency", ylim=c(0,60))
barplot(X16, ylab="Frequency", ylim=c(0,60))

describe(NFW[,15:16]) 
describeBy(NFW[,15:16], NFW$X122) # mean, median, sd by gender

# sjp.likert(NFW[ , 15:16], 
#           catcount = 4, 
#           legendLabels=c("None", "A Little", "Some", "A Lot"),
#           value.labels="sum.outside",
#           geom.colors="Blues",
#           cat.neutral.color = "Grey",
#           showPercentageSign=TRUE,
#           includeN=TRUE,
#           gridRange=1, 
#           axisLabels.y=c(
#             "a. KNOWLEDGE of active-learning strategies in physics or astronomy education?",
#             "b. SKILL in teaching using active learning?"),
#           title="Q06. How would you rate your current level of..."
#) 

X17 <- table(NFW$X17); table(NFW$X17)/sum(table(NFW$X17))
X18 <- table(NFW$X18); table(NFW$X18)/sum(table(NFW$X18)) 
X19 <- table(NFW$X19); table(NFW$X19)/sum(table(NFW$X19)) 
X20 <- table(NFW$X20); table(NFW$X20)/sum(table(NFW$X20)) 
X21 <- table(NFW$X21); table(NFW$X21)/sum(table(NFW$X21)) 

barplot(X17, ylab="Frequency", ylim=c(0,60))
barplot(X18, ylab="Frequency", ylim=c(0,60))
barplot(X19, ylab="Frequency", ylim=c(0,60))
barplot(X20, ylab="Frequency", ylim=c(0,60))
barplot(X21, ylab="Frequency", ylim=c(0,60))

describe(NFW[,17:21])
describeBy(NFW[,17:21], NFW$X122)

# sjp.likert(NFW[ , 17:21], 
#           catcount = 4, 
#           cat.neutral=0,
#           legendLabels=c("Not Very", "A Little Bit", "Somewhat", "Highly", "Don't Know"),
#           value.labels="sum.outside",
#           geom.colors="Blues",
#           cat.neutral.color = "Grey",
#           showPercentageSign=TRUE,
#           expand.grid=TRUE,
#           includeN=TRUE,
#           gridRange = 1.5,
#           axisLabels.y=c(
#             "a. How EFFECTIVE do you believe active-learning strategies are in promoting student learning?",
#             "b. How MOTIVATED do you feel to incorporate active-learning strategies into your teaching methods?  ",
#             "c. How SUPPORTED BY OTHERS do you feel in incorporating active-learning strategies into your teaching methods?",
#             "d. How confident do you feel that you could SUPPORT A COLLEAGUE in your department to incorporate active-learning strategies into their teaching methods?",
#             "e. How confident do you feel that you could GET GOOD STUDENT EVALUATIONS while incorporating active-learning strategies into your classroom?"),
#           title="Q07. Please describe your perceptions of active learning in the following questions."
#) 

X14 <- table(NFW$X14); table(NFW$X14)/sum(table(NFW$X14)) 
describe(NFW$X14)
cor(NFW[,14:21], use="complete.obs") # single scale

barplot(X14, xlab="Years", ylab="Frequency", ylim=c(0,20))

describeBy(NFW$X14, NFW$X122)
cor(NFW[,15:21], use="complete.obs") # single scale

descript(NFW[,15:21])  # single scale
