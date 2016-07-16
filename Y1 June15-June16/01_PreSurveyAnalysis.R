setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/Year 1")
source("00_MergedNFWPreSurveys.R")

#---------------------------------------------------------------------------
# 01. Demographics
#---------------------------------------------------------------------------

# Gender 
# ~~~~~~

# Frequency table of unique responses
table(Y1.NFW$Gender)/sum(table(Y1.NFW$Gender))

# Bar plot
bar <- barplot(table(Y1.NFW$Gender), 
               ylab="Frequency", ylim=c(0,150),
               names.arg=c("Male", "Female", "Prefer not \nto answer"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(Y1.NFW$Gender), 
     label = table(Y1.NFW$Gender), 
     pos = 3, cex = 0.8, col = "black")

# Institution Type 
# ~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(Y1.NFW$Institution)/sum(table(Y1.NFW$Institution))

# Bar plot
bar <- barplot(table(Y1.NFW$Institution), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Male", "Female", "Prefer not \nto answer"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(Y1.NFW$Institution), 
     label = table(Y1.NFW$Institution), 
     pos = 3, cex = 0.8, col = "black")

# Ethnicity 
# ~~~~~~~~~

# Frequency table of unique responses
table(Y1.NFW$Ethnicity)/sum(table(Y1.NFW$Ethnicity))

# Bar plot
par(mar = c(7, 4, 2, 2) + 0.9)
bar.pre <- barplot(c(2,41,2,4,6,3,104,13), 
                   ylab="Frequency", ylim=c(0,120),
                   names.arg=c("American Indian or\nAlaskan Native",
                               "Asian or\nPacific Islander", 
                               "Asian or Pacific Islander &\nWhite/Caucasian",
                               "Black or\nAfrican American",
                               "Hispanic or\nLatino",
                               "Hispanic or Latino &\nWhite/Caucasian", 
                               "White/\nCaucasian",
                               "Prefer not to\nanswer/NA"),
                   cex.names=0.9, cex.axis=0.8, las=2)
text(x = bar.pre, 
     y = c(2,41,2,4,6,3,104,13), 
     label = c(2,41,2,4,6,3,104,13), 
     pos = 3, cex = 0.8, col = "black")

# Degree 
# ~~~~~~

# Frequency table of unique responses
table(Y1.NFW$Ugrad)/sum(table(Y1.NFW$Ugrad))
table(Y1.NFW$Grad)/sum(table(Y1.NFW$Grad))

bar.pre <- barplot(matrix(c(2,1,0,0,35,4,0,0,21,12,112,153,3,1),nr=2), 
                   beside=T, 
                   col=c("white","gray50"), 
                   names.arg=c("Africa", "Antartica", "Asia", "Australia", "Europe", 
                               "North \nAmerica", "South \nAmerica"),
                   ylab="Frequency", ylim=c(0,180),
                   cex.names=0.8, cex.axis=0.8, las=2)
text(x = bar.pre, 
     y = matrix(c(2,1,0,0,35,4,0,0,21,12,112,153,3,1),nr=2), 
     label = matrix(c(2,1,NA,NA,35,4,NA,NA,21,12,112,153,3,1),nr=2), 
     pos = 3, cex = 0.8, col = "black")
legend(1, 160, 
       legend=c("Undergraduate degree (Bachelor's or equivalent)","Doctoral degree"), 
       col=c("gray10","gray50"),
       fill=c("white","gray50"),
       border="black",
       bty="n", y.intersp=0.8, cex = 0.75)

#---------------------------------------------------------------------------
# 02. Teaching Experience and Use of Active Learning
#---------------------------------------------------------------------------

describe(Y1.NFW$TchExp)
table(Y1.NFW$TchExp)

hist(Y1.NFW$TchExp, main=NA, breaks=8, labels=T, 
     ylab="Frequency", ylim=c(0,200), 
     xlab="Years of Experience", xlim=c(0,50))

bar <- barplot(c(13, 57, 39, 26, 13, 14, 4, 1, 5, 0, 0, 
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
               ylab="Frequency", ylim=c(0,70),
               xlab="Years of Experience",
               names.arg=c("0", "1", "2", "3", "4", "5", "6", "7", "8", NA, NA, 
                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "23"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = c(13, 57, 39, 26, 13, 14, 4, 1, 5, 0, 0, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
     label = c(13, 57, 39, 26, 13, 14, 4, 1, 5, NA, NA, 
               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1), 
     pos = 3, cex = 0.8, col = "black")


# Number Who Have Taught a Course 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(Y1.NFW$TaughtCourse)/sum(table(Y1.NFW$TaughtCourse))

# Bar plot
bar <- barplot(table(Y1.NFW$TaughtCourse), 
               ylab="Frequency", ylim=c(0,200),
               names.arg=c("I have taught\na course", 
                           "I have NOT taught\na course"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(Y1.NFW$TaughtCourse), 
     label = table(Y1.NFW$TaughtCourse), 
     pos = 3, cex = 0.8, col = "black")

# Time Spent on Active Learning 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(Y1.NFW$ClassTime)/sum(table(Y1.NFW$ClassTime))

# Histogram of time spent on active learning
bar <- barplot(table(Y1.NFW$ClassTime), 
               ylab="Frequency", ylim=c(0,60),
               names.arg=c("Low", "Medium", "High"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(Y1.NFW$ClassTime), 
     label = table(Y1.NFW$ClassTime), 
     pos = 3, cex = 0.8, col = "black")



# PIPS
#~~~~~

# Cronbach's Alpha 
descript(Y1.NFW[, c(7,9,11:15,17:21,23:25)])  # student-centered
descript(Y1.NFW[, c(6,8,10,16,22,26:29)])  # instructor-centered

# Convert any NAs to Os
Y1.NFW[,c(6:29)][is.na(Y1.NFW[,c(6:29)])] <- 0

# Create a new variable for the total score for both constructs
Y1.NFW$C1 <- Y1.NFW$PIPS.C1.1 + Y1.NFW$PIPS.C1.2 + Y1.NFW$PIPS.C1.3 + Y1.NFW$PIPS.C1.4 +
  Y1.NFW$PIPS.C1.5 + Y1.NFW$PIPS.C1.6 + Y1.NFW$PIPS.C1.7 + Y1.NFW$PIPS.C1.8 +
  Y1.NFW$PIPS.C1.9 + Y1.NFW$PIPS.C1.10 + Y1.NFW$PIPS.C1.11 + Y1.NFW$PIPS.C1.12 +
  Y1.NFW$PIPS.C1.13 + Y1.NFW$PIPS.C1.14 + Y1.NFW$PIPS.C1.15
Y1.NFW$C1 <- (Y1.NFW$C1/75)*100
Y1.NFW$C2 <- Y1.NFW$PIPS.C2.1 + Y1.NFW$PIPS.C2.2 + Y1.NFW$PIPS.C2.3 + Y1.NFW$PIPS.C2.4 + 
  Y1.NFW$PIPS.C2.5 + Y1.NFW$PIPS.C2.6 + Y1.NFW$PIPS.C2.7 + Y1.NFW$PIPS.C2.8 + 
  Y1.NFW$PIPS.C2.9
Y1.NFW$C2 <- (Y1.NFW$C2/45)*100

# Remove non-respondents
Y1.NFW[,35:36][Y1.NFW[,35:36]==0] <- NA  

# Get mean, median, and sd of total score
describe(Y1.NFW[,35:36])

# Histograms of the total scores
par(mar=c(5,5,5,2), mfrow=c(1,2))
hist(Y1.NFW$C1, 
     main="Distribution of Raw Scores for \n Student-Centered Practice", 
     xlab="Total Score \n (Percent out of 75 Possible Points)", xlim=c(0,100),
     ylim=c(0,80),
     labels=TRUE, col="grey")
hist(Y1.NFW$C2, breaks=10,
     main="Distribution of Raw Scores for \n Instructor-Centered Practice", 
     xlab="Total Score \n (Percent out of 45 Possible Points)", xlim=c(0,100),
     ylim=c(0,80),
     labels=TRUE, col="grey")
