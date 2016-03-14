setwd("~/Dropbox/EFW Analyses/Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 02. Teaching Experience and Use of Active Learning
#---------------------------------------------------------------------------

# Leadership 
# ~~~~~~~~~~

# Recode 0 to 3 for Question6
EFW$X16 <- ifelse(EFW$X16==0,3,EFW$X16)

# Frequency table of unique responses
table(EFW$X16)/sum(table(EFW$X16))

# Barplot
bar <- barplot(table(EFW$X16), 
               ylab="Frequency", ylim=c(0,20),
               names.arg=c("No", 
                           "Yes, \nDepartment Chair", 
                           "Yes, \nOther leadership role"),
               cex.names=0.7, cex.axis=0.8)
text(x = bar, 
     y = table(EFW$X16), 
     label = table(EFW$X16), 
     pos = 3, cex = 0.8, col = "black")

# Years of Teaching Experience 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract just the numbers from the open-ended question 7
exp <- as.numeric(unlist(regmatches(EFW$X18, gregexpr('\\(?[0-9,.]+', EFW$X18))))

# Frequency table of unique responses
table(exp); sum(table(exp))

# Histogram of years of teaching experience
hist(exp, 
     xlab="Years of Experience", xlim=c(0,50),
     ylab="Frequency", ylim=c(0,15),
     main=NA,
     labels=TRUE)

# Mean, median, SD
describe(exp)

# Time Spent on Active Learning 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(EFW$X19)/sum(table(EFW$X19))

# Histogram of years of teaching experience
hist(EFW$X19, 
     xlab="Percent of class time spent on active learning", xlim=c(0,100),
     ylab="Frequency", ylim=c(0,15),
     main=NA,
     labels=TRUE)

# Mean, median, SD
describe(EFW$X19)

