setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 02. Teaching Experience and Use of Active Learning
#---------------------------------------------------------------------------

# Years of Teaching Experience 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(Jun16.NFW.pre$X16)/sum(table(Jun16.NFW.pre$X16))

d <- merge(Jun16.NFW.pre[,c(9,11)], Jun16.NFW.post[,c(9,65)], by="Email", all.y=TRUE)
d$TchExp <- ifelse(is.na(d$X69), d$X16, d$X69)
table(d$TchExp)/sum(table(d$TchExp))

# Barplot of years of teaching experience
bar <- barplot(c(1, 15, 12, 6, 5, 2, 1, 1, 3, 0, 0, 
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                 0, 0, 1), 
               ylab="Frequency", ylim=c(0,20),
               xlab="Years of Experience",
               names.arg=c("0", "1", "2", "3", "4", "5", "6", "7", "8", NA, NA, 
                            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                            NA, NA, "23"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = c(1, 15, 12, 6, 5, 2, 1, 1, 3, 0, 0, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
           0, 0, 1), 
     label = c(1, 15, 12, 6, 5, 2, 1, 1, 3, NA, NA, 
               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
               NA, NA, 1), 
     pos = 3, cex = 0.8, col = "black")

bar <- barplot(c(2, 12, 11, 6, 3, 2, 2, 1, 2, 0, 0, 
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
               ylab="Frequency", ylim=c(0,20),
               xlab="Years of Experience",
               names.arg=c("0", "1", "2", "3", "4", "5", "6", "7", "8", NA, NA, 
                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "22"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = c(2, 12, 11, 6, 3, 2, 2, 1, 2, 0, 0, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
     label = c(2, 12, 11, 6, 3, 2, 2, 1, 2, NA, NA, 
               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1), 
     pos = 3, cex = 0.8, col = "black")

# Mean, median, SD
describe(Jun16.NFW.pre$X16)
describe(d$TchExp)

# Number Who Have Taught a Course 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(Jun16.NFW.pre$X52)/sum(table(Jun16.NFW.pre$X52))

d <- merge(Jun16.NFW.pre[,c(9,44)], Jun16.NFW.post[,9:10], by="Email", all.y=TRUE)
table(d$X52)/sum(table(d$X52))

# Bar plot
bar <- barplot(table(Jun16.NFW.pre$X52), 
               ylab="Frequency", ylim=c(0,50),
               names.arg=c("I have taught\na course", 
                           "I have NOT taught\na course"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(Jun16.NFW.pre$X52), 
     label = table(Jun16.NFW.pre$X52), 
     pos = 3, cex = 0.8, col = "black")

bar <- barplot(table(d$X52), 
               ylab="Frequency", ylim=c(0,50),
               names.arg=c("I have taught\na course", 
                           "I have NOT taught\na course"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(d$X52), 
     label = table(d$X52), 
     pos = 3, cex = 0.8, col = "black")

# Class Time Spent on Active Learning 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(Jun16.NFW.pre$X53)/sum(table(Jun16.NFW.pre$X53))

d <- merge(Jun16.NFW.pre[,c(9,45)], Jun16.NFW.post[,c(9,66)], by="Email", all.y=TRUE)
d$ClassTime <- ifelse(is.na(d$X70), d$X53, d$X70)
table(d$ClassTime)/sum(table(d$ClassTime))

# Histogram of class time spent on active learning
hist(Jun16.NFW.pre$X53,
     xlab="Percent of class time spent on active learning", xlim=c(0,100),
     ylab="Frequency", ylim=c(0,20),
     main=NA,
     labels=TRUE)

hist(d$ClassTime,
     xlab="Percent of class time spent on active learning", xlim=c(0,100),
     ylab="Frequency", ylim=c(0,20),
     main=NA,
     labels=TRUE)

# Mean, median, SD
describe(Jun16.NFW.pre$X53)
describe(d$ClassTime)


# Matched Sample 
# ~~~~~~~~~~~~~~

Jun16.NFW.pp$TchExp <- ifelse(is.na(Jun16.NFW.pp$X69.y), Jun16.NFW.pp$X16.x, Jun16.NFW.pp$X69.y)
describe(Jun16.NFW.pp$TchExp)

Jun16.NFW.pp$TaughtCourse <- Jun16.NFW.pp$X52.x
table(Jun16.NFW.pp$TaughtCourse)/sum(table(Jun16.NFW.pp$TaughtCourse))

Jun16.NFW.pp$ClassTime <- ifelse(is.na(Jun16.NFW.pp$X70.y), Jun16.NFW.pp$X53.x, Jun16.NFW.pp$X70.y)
describe(Jun16.NFW.pp$ClassTime)
