setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 EFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 05. Knowledge and Beliefs
#---------------------------------------------------------------------------

# Average Scores in March 2016 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get means for March 2016 workshop
describe(EFW.pre[,16:22])
EFW.post[,c(50:54)][EFW.post[,c(50:54)]==5] <- 0
describe(EFW.post[,48:54])

# Barplots
barplot(table(EFW.pre[,16]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW.pre[,17]), ylab="Frequency", ylim=c(0,40))
barplot(c(2,0,1,17,20), names.arg=c(0,NA,2,3,4), ylab="Frequency", ylim=c(0,40))
barplot(c(0, 0, table(EFW.pre[,19])), ylab="Frequency", ylim=c(0,40))
barplot(c(0, table(EFW.pre[,20])), ylab="Frequency", ylim=c(0,40))
barplot(c(0, 0, table(EFW.pre[,21])), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW.pre[,22]), ylab="Frequency", ylim=c(0,40))

barplot(c(0, table(EFW.post[,48])), ylab="Frequency", ylim=c(0,30))
barplot(table(EFW.post[,49]), ylab="Frequency", ylim=c(0,30))
barplot(c(1,0,0,10,22), names.arg=c(0,NA,NA,3,4), ylab="Frequency", ylim=c(0,30))
barplot(c(0,0,0,table(EFW.post[,51])), ylab="Frequency", ylim=c(0,30))
barplot(c(1,0,5,15,11), names.arg=c(0,NA,2,3,4), ylab="Frequency", ylim=c(0,30))
barplot(c(0,0,table(EFW.post[,53])), ylab="Frequency", ylim=c(0,30))
barplot(c(1,0,4,13,15), names.arg=c(0,NA,2,3,4), ylab="Frequency", ylim=c(0,30))

# Create a matrix of medians across workshops
cols <- c("Knowledge", "Skill", "Effective", "Motivated", "Support Others", 
          "Support Colleague", "Evaluations")
J15.medpre <- c(3.0, 2.0, 3.0, 3.0, NA, NA, NA)
N15.medpre <- c(3.0, 2.0, 4.0, 4.0, 3.0, 3.0, 3.0)
M16.medpre <- c(3.0, 3.0, 3.5, 3.5, 3.0, 3.0, 3.0)
d.medpre <- rbind(J15.medpre, N15.medpre, M16.medpre)  
colnames(d.medpre)[1:7] <- cols
barplot(as.matrix(d.medpre), 
        ylab = "Median Score", ylim=c(0,5),
        xlab = "Common Question",
        main = "Pre-Survey",
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
text(23.2, 4.5, "Note: SDs are above bars.", cex=0.55)
text(17.5, 0.3, "NA", cex=0.55)
text(21.5, 0.3, "NA", cex=0.55)
text(25.5, 0.3, "NA", cex=0.55)
text(1.55, 3.2, "(0.7)", cex=0.5)
text(2.55, 3.2, "(0.7)", cex=0.5)
text(3.55, 3.2, "(0.6)", cex=0.5)
text(5.5, 2.2, "(0.6)", cex=0.5)
text(6.5, 2.2, "(0.8)", cex=0.5)
text(7.55, 3.2, "(0.7)", cex=0.5)
text(9.5, 3.2, "(1.2)", cex=0.5)
text(10.55, 4.2, "(0.4)", cex=0.5)
text(11.55, 3.7, "(0.9)", cex=0.5)
text(13.5, 3.2, "(0.8)", cex=0.5)
text(14.55, 4.2, "(0.8)", cex=0.5)
text(15.55, 3.7, "(0.6)", cex=0.5)
text(18.55, 3.2, "(0.8)", cex=0.5)
text(19.55, 3.2, "(0.8)", cex=0.5)
text(22.55, 3.2, "(0.9)", cex=0.5)
text(23.55, 3.2, "(0.6)", cex=0.5)
text(26.55, 3.2, "(0.7)", cex=0.5)
text(27.55, 3.2, "(1.3)", cex=0.5)

J15.medpost <- c(3.0, 2.0, 4.0, 4.0, NA, NA, NA)
N15.medpost <- c(3.0, 3.0, 4.0,  NA, 3.0, 3.0, 3.0)
M16.medpost <- c(3.0, 3.0, 4.0, 4.0, 3.0, 4.0, 3.0)
d.medpost <- rbind(J15.medpost, N15.medpost, M16.medpost)  
colnames(d.medpost)[1:7] <- cols
barplot(as.matrix(d.medpost), 
        ylab = "Median Score", ylim=c(0,5),
        xlab = "Common Question",
        main = "Post-Survey",
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
text(23.2, 4.5, "Note: SDs are above bars.", cex=0.55)
text(14.5, 0.3, "NA", cex=0.55)
text(17.5, 0.3, "NA", cex=0.55)
text(21.5, 0.3, "NA", cex=0.55)
text(25.5, 0.3, "NA", cex=0.55)
text(1.55, 3.2, "(0.7)", cex=0.5)
text(2.55, 3.2, "(0.6)", cex=0.5)
text(3.55, 3.2, "(0.6)", cex=0.5)
text(5.5, 2.2, "(0.6)", cex=0.5)
text(6.55, 3.2, "(0.5)", cex=0.5)
text(7.55, 3.2, "(0.7)", cex=0.5)
text(9.5, 4.2, "(0.4)", cex=0.5)
text(10.55, 4.2, "(0.6)", cex=0.5)
text(11.55, 4.2, "(0.8)", cex=0.5)
text(13.5, 4.2, "(0.5)", cex=0.5)
text(15.55, 4.2, "(0.5)", cex=0.5)
text(18.55, 3.2, "(0.8)", cex=0.5)
text(19.55, 3.2, "(0.9)", cex=0.5)
text(22.55, 3.2, "(0.7)", cex=0.5)
text(23.55, 4.2, "(0.6)", cex=0.5)
text(26.55, 3.2, "(1.0)", cex=0.5)
text(27.55, 3.2, "(0.9)", cex=0.5)

d.medM16 <- rbind(M16.medpre, M16.medpost)
colnames(d.medM16)[1:7] <- cols
barplot(as.matrix(d.medM16), 
        ylab = "Median Score", ylim=c(0,5),
        xlab = "Common Question",
        main = "March 2016 Pre- and Post-Survey",
        cex.lab = 0.9,
        beside=TRUE, col=c("gray90", "gray30") ,
        names.arg=c("Knowledge", 
                    "Skill", 
                    "Effective", 
                    "Motivated",
                    "Support\nOthers", 
                    "Support\nColleague",
                    "Evaluations"), cex.names = 0.9)
legend("topright", 
       c("Pre-Survey","Post-Survey"), 
       cex=0.55, bty="n", fill=c("gray90", "gray30"), ncol=2)
text(18.8, 4.5, "Note: SDs are above bars.", cex=0.55)
text(1.5, 3.2, "(0.6)", cex=0.5)
text(2.5, 3.2, "(0.6)", cex=0.5)
text(4.5, 3.2, "(0.7)", cex=0.5)
text(5.5, 3.2, "(0.7)", cex=0.5)
text(7.5, 3.7, "(0.9)", cex=0.5)
text(8.5, 4.2, "(0.8)", cex=0.5)
text(10.5, 3.7, "(0.6)", cex=0.5)
text(11.5, 4.2, "(0.5)", cex=0.5)
text(13.5, 3.2, "(0.8)", cex=0.5)
text(14.5, 3.2, "(0.9)", cex=0.5)
text(16.5, 3.2, "(0.6)", cex=0.5)
text(17.5, 4.2, "(0.6)", cex=0.5)
text(19.5, 3.2, "(1.3)", cex=0.5)
text(20.5, 3.2, "(0.9)", cex=0.5)




X51 <- sum(table(EFW.post$X51)); prop.table(X51)






















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






###

# Create a matrix of rounded averages across workshops
J15.a <- c(3.0, 2.0, 3.0, 3.0, NA, NA, NA)
N15.a <- c(3.0, 2.0, 4.0, 4.0, 3.0, 3.0, 3.0)
M16.a <- c(3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0)
d.a <- rbind(J15.a, N15.a, M16.a)  
colnames(d.a)[1:7] <- c("Knowledge", 
                      "Skill", 
                      "Effective", 
                      "Motivated",
                      "Support Others", 
                      "Support Colleague",
                      "Evaluations")




# Barplot
barplot(as.matrix(d.a), 
        ylab = "Average Score (Rounded)", ylim=c(0,5),
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



