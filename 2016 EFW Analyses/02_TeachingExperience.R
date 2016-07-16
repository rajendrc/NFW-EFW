setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 EFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 02. Teaching Experience and Use of Active Learning
#---------------------------------------------------------------------------

# Leadership 
# ~~~~~~~~~~

# Recode 0 to 3 for Questions 6 and 19
EFW.pre$X16 <- ifelse(EFW.pre$X16==0,3,EFW.pre$X16)
EFW.post$X61 <- ifelse(EFW.post$X61==0,3,EFW.post$X61)

# Frequency table of unique responses
table(EFW.pre$X16)/sum(table(EFW.pre$X16))
table(EFW.post$X61)/sum(table(EFW.post$X61))
table(EFW.pp$X16.x)/sum(table(EFW.pp$X16.x))
table(EFW.pp$X61.y)/sum(table(EFW.pp$X61.y))
View(EFW.pp[,c(1:10,12:13,138:139)])

# Barplots
bar.pre <- barplot(table(EFW.pre$X16), 
               ylab="Frequency", ylim=c(0,20),
               names.arg=c("No", 
                           "Yes, \nDepartment Chair", 
                           "Yes, \nOther leadership role"),
               cex.names=0.7, cex.axis=0.8)
text(x = bar.pre, 
     y = table(EFW.pre$X16), 
     label = table(EFW.pre$X16), 
     pos = 3, cex = 0.8, col = "black")

bar.post <- barplot(table(EFW.post$X61), 
                   ylab="Frequency", ylim=c(0,20),
                   names.arg=c("No", 
                               "Yes, \nDepartment Chair", 
                               "Yes, \nOther leadership role"),
                   cex.names=0.7, cex.axis=0.8)
text(x = bar.post, 
     y = table(EFW.post$X61), 
     label = table(EFW.post$X61), 
     pos = 3, cex = 0.8, col = "black")


# Years of Teaching Experience 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract just the numbers from the open-ended question 7
exp.pre <- as.numeric(unlist(regmatches(EFW.pre$X18, gregexpr('\\(?[0-9,.]+', EFW.pre$X18))))
exp.pp <- as.numeric(unlist(regmatches(EFW.pp$X18.x, gregexpr('\\(?[0-9,.]+', EFW.pp$X18.x))))

# Frequency table of unique responses
table(exp.pre); sum(table(exp.pre))
table(exp.pp); sum(table(exp.pp))

# Histogram of years of teaching experience
hist(exp.pre, 
     xlab="Years of Experience", xlim=c(0,50),
     ylab="Frequency", ylim=c(0,15),
     main=NA,
     labels=TRUE)
hist(exp.pp, 
     xlab="Years of Experience", xlim=c(0,50),
     ylab="Frequency", ylim=c(0,15),
     main=NA,
     labels=TRUE)

# Mean, median, SD
describe(exp)
describe(exp.pp)

# Time Spent on Active Learning 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(EFW.pre$X19)/sum(table(EFW.pre$X19))
table(EFW.post$X63)/sum(table(EFW.post$X63))
table(EFW.pp$X19.x)
table(EFW.pp$X63.y)
EFW.pp$timediff <- EFW.pp$X63.y-EFW.pp$X19.x
View(EFW.pp[,c(1:10,15,140,148)])
table(EFW.pp$timediff)
hist(EFW.pp$timediff,
     xlab="Difference Between Pre- and Post-Responses to\nTime Spent on Active Learning", xlim=c(-60,60),
     ylab="Frequency", ylim=c(0,15),
     main=NA,
     labels=TRUE)

EFW.pre$ClassTime <- NA
EFW.pre$ClassTime <- ifelse(EFW.pre$X19<=10, 1, EFW.pre$ClassTime)
EFW.pre$ClassTime <- ifelse(EFW.pre$X19>10 & EFW.pre$X19<=20, 2, EFW.pre$ClassTime)
EFW.pre$ClassTime <- ifelse(EFW.pre$X19>20, 3, EFW.pre$ClassTime)

table(EFW.pre$ClassTime)/sum(table(EFW.pre$ClassTime))

bar <- barplot(table(EFW.pre$ClassTime), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Low", "Medium", "High"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(EFW.pre$ClassTime), 
     label = table(EFW.pre$ClassTime), 
     pos = 3, cex = 0.8, col = "black")


# Histogram of years of teaching experience
hist(EFW.pre$X19, 
     xlab="Percent of class time spent on active learning", xlim=c(0,100),
     ylab="Frequency", ylim=c(0,15),
     main=NA,
     labels=TRUE)
hist(EFW.post$X63, 
     xlab="Percent of class time spent on active learning", xlim=c(0,100),
     ylab="Frequency", ylim=c(0,15),
     main=NA,
     labels=TRUE)
hist(EFW.pp$X19.x, 
     xlab="Percent of class time\nspent on active learning", xlim=c(0,100),
     ylab="Frequency", ylim=c(0,15),
     main=NA,
     labels=TRUE)
hist(EFW.pp$X63.y, 
     xlab="Percent of class time\nspent on active learning", xlim=c(0,100),
     ylab="Frequency", ylim=c(0,15),
     main=NA,
     labels=TRUE)

# Mean, median, SD
describe(EFW.pre$X19)
describe(EFW.post$X63)
describe(EFW.pp$X19.x); describe(EFW.pp$X63.y)
