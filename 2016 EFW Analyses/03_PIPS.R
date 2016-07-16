setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 EFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 03. PIPS
#---------------------------------------------------------------------------

# Student- and Instructor-Centered Practice 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cronbach's Alpha 
descript(NFW[, c(47, 49, 51:60, 62:66, 68:70)])  # student-centered
descript(NFW[, c(46, 48, 50, 61, 67, 71:74)])  # instructor-centered

# Convert any NAs to Os
EFW[,c(51:74)][is.na(EFW[,c(51:74)])] <- 0

# Create a new variable for the total score for both constructs
EFW$C1 <- EFW$X56 + EFW$X58 + EFW$X60 + EFW$X61 + EFW$X62 + EFW$X63 + 
  EFW$X64 + EFW$X66 + EFW$X67 + EFW$X68 + EFW$X69 + EFW$X70 + EFW$X72 +
  EFW$X73 + EFW$X74
EFW$C1 <- (EFW$C1/75)*100
EFW$C2 <- EFW$X55 + EFW$X57 + EFW$X59 + EFW$X65 + EFW$X71 + EFW$X75 + 
  EFW$X76 + EFW$X77 + EFW$X78
EFW$C2 <- (EFW$C2/45)*100

# Get mean, median, and sd of total score
describe(EFW[,83:84])

# Convert any 0's for total scores back to NAs
EFW[,c(83)] <- ifelse(EFW[,c(83)]==0, NA, EFW[,c(83)])
EFW[,c(84)] <- ifelse(EFW[,c(84)]==0, NA, EFW[,c(84)])

# Histograms of the total scores
par(mar=c(5,5,5,2), mfrow=c(1,2))
hist(EFW$C1, 
     main="Distribution of Raw Scores for \n Student-Centered Practice", 
     xlab="Total Score \n (Percent out of 75 Possible Points)", xlim=c(0,100),
     ylim=c(0,20),
     labels=TRUE, col="grey")
hist(EFW$C2, breaks=5,
     main="Distribution of Raw Scores for \n Instructor-Centered Practice", 
     xlab="Total Score \n (Percent out of 45 Possible Points)", xlim=c(0,100),
     ylim=c(0,20),
     labels=TRUE, col="grey")
