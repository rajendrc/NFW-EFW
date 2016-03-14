setwd("~/Dropbox/EFW Analyses/Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 05. Knowledge and Beliefs
#---------------------------------------------------------------------------

# Average Scores in March 2016 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get means for March 2016 workshop
describe(EFW[,16:22])

# Barplots
barplot(table(EFW[,16]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,17]), ylab="Frequency", ylim=c(0,40))
barplot(c(2,0,1,17,20), names.arg=c(0,NA,2,3,4), ylab="Frequency", ylim=c(0,40))
barplot(c(0, 0, table(EFW[,19])), ylab="Frequency", ylim=c(0,40))
barplot(c(0, table(EFW[,20])), ylab="Frequency", ylim=c(0,40))
barplot(c(0, 0, table(EFW[,21])), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,22]), ylab="Frequency", ylim=c(0,40))

# Create a matrix of averages across workshops
J15 <- c(2.6, 2.3, 3.0, 3.2, NA, NA, NA)
N15 <- c(2.7, 2.3, 3.5, 3.5, 3.3, 2.8, 2.8)
M16 <- c(3.1, 2.8, 3.3, 3.4, 3.2, 3.4, 2.7)
d <- rbind(J15, N15, M16)  
colnames(d)[1:7] <- c("Knowledge", 
                      "Skill", 
                      "Effective", 
                      "Motivated",
                      "Support Others", 
                      "Support Colleague",
                      "Evaluations")

# Barplot
barplot(as.matrix(d), 
        ylab = "Average Score", ylim=c(0,5),
        xlab = "Common Question",
        cex.lab = 0.9,
        beside=TRUE, col=c("gray90", "gray60", "gray30") ,
        names.arg=c("Knowledge", 
                    "Skill", 
                    "Effective", 
                    "Motivated",
                    "Support\nOthers", 
                    "Support\nColleague",
                    "Evaluations"), cex.names = 0.9)
legend("topright", 
       c("Jun 2015","Nov 2015","Mar 2016"), 
       cex=0.55, bty="n", fill=c("gray90", "gray60", "gray30"), ncol=3)
text(17.5, 0.3, "NA", cex=0.55)
text(21.5, 0.3, "NA", cex=0.55)
text(25.5, 0.3, "NA", cex=0.55)
abline(h=0,col=1,lty=1)

