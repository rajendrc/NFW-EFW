setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 03. PIPS
#---------------------------------------------------------------------------

# Student- and Instructor-Centered Practice 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cronbach's Alpha 
descript(NFW.pre[, c(47, 49, 51:55, 57:61, 63:65)])  # student-centered
descript(NFW.pre[, c(46, 48, 50, 56, 62, 66:69)])  # instructor-centered

# Convert any NAs to Os
# NFW.pre[,c(46:69)][is.na(NFW.pre[,c(46:69)])] <- 0

# Create a new variable for the total score for both constructs
NFW.pre$C1 <- NFW.pre$X55 + NFW.pre$X57 + NFW.pre$X59 + NFW.pre$X60 + NFW.pre$X61 + 
  NFW.pre$X62 + NFW.pre$X63 + NFW.pre$X65 + NFW.pre$X66 + NFW.pre$X67 + NFW.pre$X68 + 
  NFW.pre$X69 + NFW.pre$X71 + NFW.pre$X72 + NFW.pre$X73
NFW.pre$C1 <- (NFW.pre$C1/75)*100
NFW.pre$C2 <- NFW.pre$X54 + NFW.pre$X56 + NFW.pre$X58 + NFW.pre$X64 + NFW.pre$X70 + 
  NFW.pre$X74 + NFW.pre$X75 + NFW.pre$X76 + NFW.pre$X77
NFW.pre$C2 <- (NFW.pre$C2/45)*100

# Get mean, median, and sd of total score
describe(NFW.pre[,82:83])

# Histograms of the total scores
par(mar=c(5,5,5,2), mfrow=c(1,2))
hist(NFW.pre$C1, 
     main="Distribution of Raw Scores for \n Student-Centered Practice", 
     xlab="Total Score \n (Percent out of 75 Possible Points)", xlim=c(0,100),
     ylim=c(0,20),
     labels=TRUE, col="grey")
hist(NFW.pre$C2, breaks=5,
     main="Distribution of Raw Scores for \n Instructor-Centered Practice", 
     xlab="Total Score \n (Percent out of 45 Possible Points)", xlim=c(0,100),
     ylim=c(0,20),
     labels=TRUE, col="grey")

# Barplot of Means Across Workshops

par(mfrow = c(1, 2), mar=c(5,6,2,2) )
bar1 <- barplot(c(64.8, 64.3, 61.5, 60.8), 
               ylab="Average % PIPS Score\n(SD)", ylim=c(0,100),
               names.arg=c("June\n2015","November\n2015","March\n2016","June\n2016"),
               cex.names=0.8, cex.axis=0.8, cex.main=1, las=2,
               main="Student-Centered Practice")
text(x = bar1, 
     y = c(64.8, 64.3, 61.5, 60.8), 
     label = c("64.8\n(13.7)", "64.3\n(13.1)", "61.5\n(15.5)", "60.8\n(11.8)"), 
     pos = 3, cex = 0.8, col = "black")

bar2 <- barplot(c(67.9, 59.3, 66.7, 65.7), 
               ylab="Average % PIPS Score\n(SD)", ylim=c(0,100),
               names.arg=c("June\n2015","November\n2015","March\n2016","June\n2016"),
               cex.names=0.8, cex.axis=0.8, cex.main=1, las=2,
               main="Teacher-Centered Practice")
text(x = bar2, 
     y = c(67.9, 59.3, 66.7, 65.7), 
     label = c("67.9\n(9.9)", "59.3\n(8.1)", "66.7\n(9.8)", "65..7\n(8.4)"), 
     pos = 3, cex = 0.8, col = "black")

