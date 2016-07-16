setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 05. Knowledge and Beliefs
#---------------------------------------------------------------------------

# Average Scores in June 2016 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get means for June 2016 workshop
describe(Jun16.NFW.pre[,12:18])

# Barplots
barplot(table(NFW.pre[,12]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,13]), ylab="Frequency", ylim=c(0,40))
barplot(c(3,0,3,16,25), names.arg=c(0,NA,2,3,4), ylab="Frequency", ylim=c(0,40))
barplot(c(1,0,2,17,27), names.arg=c(0,NA,2,3,4), ylab="Frequency", ylim=c(0,40))
barplot(c(1,0,7,14,25), names.arg=c(0,NA,2,3,4), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,17]), ylab="Frequency", ylim=c(0,40))
barplot(c(5,0,9,21,12), names.arg=c(0,NA,2,3,4), ylab="Frequency", ylim=c(0,40))


# Create a matrix of medians across workshops
cols <- c("Knowledge", "Skill", "Effective", "Motivated", "Support Others", 
          "Support Colleague", "Evaluations")
J15.medpre <- c(3.0, 2.0, 3.0, 3.0, NA, NA, NA)
N15.medpre <- c(3.0, 2.0, 4.0, 4.0, 3.0, 3.0, 3.0)
M16.medpre <- c(3.0, 3.0, 3.5, 3.5, 3.0, 3.0, 3.0)
J16.medpre <- c(3.0, 2.0, 4.0, 4.0, 4.0, 3.0, 3.0)
d.medpre <- rbind(J15.medpre, N15.medpre, M16.medpre, J16.medpre)  
colnames(d.medpre)[1:7] <- cols
barplot(as.matrix(d.medpre), 
        ylab = "Median Score", ylim=c(0,4.9),
        main = "Pre-Survey",
        cex.lab = 0.9,
        beside=TRUE, col=c("gray80", "gray60", "gray40", "gray 20") ,
        names.arg=c("Knowledge", 
                    "Skill", 
                    "Effective", 
                    "Motivated",
                    "Support\nOthers", 
                    "Support\nColleague",
                    "Evaluations"), cex.names = 0.9, las=1)
legend("topleft", 
       c("Jun 2015","Nov 2015","Mar 2016", "Jun 2016"), 
       cex=0.55, bty="n", fill=c("gray80", "gray60", "gray40", "gray 20"), ncol=2)
text(2.2, 4.2, "Note: SDs are above bars.", cex=0.55)
text(21.5, 0.2, "NA", cex=0.55)
text(26.5, 0.2, "NA", cex=0.55)
text(31.5, 0.2, "NA", cex=0.55)
text(1.5, 3.2, "(0.7)", cex=0.5)
text(2.5, 3.2, "(0.7)", cex=0.5)
text(3.5, 3.2, "(0.6)", cex=0.5)
text(4.5, 3.2, "(0.8)", cex=0.5)
text(6.5, 2.2, "(0.6)", cex=0.5)
text(7.5, 2.2, "(0.8)", cex=0.5)
text(8.5, 3.2, "(0.7)", cex=0.5)
text(9.5, 2.2, "(0.8)", cex=0.5)
text(11.5, 3.2, "(1.2)", cex=0.5)
text(12.5, 4.2, "(0.4)", cex=0.5)
text(13.5, 3.7, "(0.9)", cex=0.5)
text(14.5, 4.2, "(1.1)", cex=0.5)
text(16.5, 3.2, "(0.8)", cex=0.5)
text(17.5, 4.2, "(0.8)", cex=0.5)
text(18.5, 3.7, "(0.6)", cex=0.5)
text(19.5, 4.2, "(0.8)", cex=0.5)
text(22.5, 3.2, "(0.8)", cex=0.5)
text(23.5, 3.2, "(0.8)", cex=0.5)
text(24.5, 4.2, "(0.9)", cex=0.5)
text(27.5, 3.2, "(0.9)", cex=0.5)
text(28.5, 3.2, "(0.6)", cex=0.5)
text(29.5, 3.2, "(1.2)", cex=0.5)
text(32.5, 3.2, "(0.7)", cex=0.5)
text(33.5, 3.2, "(1.3)", cex=0.5)
text(34.5, 3.2, "(1.2)", cex=0.5)

# Post-Workshop

describe(Jun16.NFW.post[,10:16])

barplot(table(Jun16.NFW.post[,10]), ylab="Frequency", ylim=c(0,40))
barplot(table(Jun16.NFW.post[,11]), ylab="Frequency", ylim=c(0,40))

table(Jun16.NFW.post[,12]); Eff <- c("0"=1, 0, 0, "3"=4, "4"=40)
table(Jun16.NFW.post[,13]); Mot <- c(0, 0, 0, "3"=6, "4"=39)
table(Jun16.NFW.post[,14]); Oth <- c("0"=2, 0, "2"=3, "3"=23, "4"=17)
table(Jun16.NFW.post[,15]); Col <- c(0, "1"=2, "2"=2, "3"=22, "4"=19)
table(Jun16.NFW.post[,16]); Evl <- table(Jun16.NFW.post[,16])

barplot(Eff, ylab="Frequency", ylim=c(0,40))
barplot(Mot, ylab="Frequency", ylim=c(0,40))
barplot(Oth, ylab="Frequency", ylim=c(0,40))
barplot(Col, ylab="Frequency", ylim=c(0,40))
barplot(Evl, ylab="Frequency", ylim=c(0,40))

# Create a matrix of medians across workshops
cols <- c("Knowledge", "Skill", "Effective", "Motivated", "Support Others", 
          "Support Colleague", "Evaluations")
J15.medpost <- c(3.0, 2.0, 4.0, 4.0, NA, NA, NA)
N15.medpost <- c(3.0, 3.0, 4.0, NA, 3.0, 3.0, 3.0)
M16.medpost <- c(3.0, 3.0, 4.0, 4.0, 3.0, 4.0, 3.0)
J16.medpost <- c(3.0, 2.0, 4.0, 4.0, 3.0, 3.0, 3.0)
d.medpost <- rbind(J15.medpost, N15.medpost, M16.medpost, J16.medpost)  
colnames(d.medpost)[1:7] <- cols
barplot(as.matrix(d.medpost), 
        ylab = "Median Score", ylim=c(0,4.9),
        main = "Post-Survey",
        cex.lab = 0.9,
        beside=TRUE, col=c("gray80", "gray60", "gray40", "gray 20") ,
        names.arg=c("Knowledge", 
                    "Skill", 
                    "Effective", 
                    "Motivated",
                    "Support\nOthers", 
                    "Support\nColleague",
                    "Evaluations"), cex.names = 0.9, las=1)
legend("topleft", 
       c("Jun 2015","Nov 2015","Mar 2016", "Jun 2016"), 
       cex=0.55, bty="n", fill=c("gray80", "gray60", "gray40", "gray 20"), ncol=2)
text(2.2, 4.2, "Note: SDs are above bars.", cex=0.55)
text(1.5, 3.2, "(0.7)", cex=0.5) # Jun 15
text(6.5, 2.2, "(0.6)", cex=0.5)
text(11.5, 4.2, "(0.4)", cex=0.5)
text(16.5, 4.2, "(0.5)", cex=0.5)
text(21.5, 0.2, "NA", cex=0.55)
text(26.5, 0.2, "NA", cex=0.55)
text(31.5, 0.2, "NA", cex=0.55)
text(2.5, 3.2, "(0.6)", cex=0.5) # Nov 15
text(7.5, 3.2, "(0.5)", cex=0.5)
text(12.5, 4.2, "(0.6)", cex=0.5)
text(17.5, 0.2, "NA", cex=0.5)
text(22.5, 3.2, "(0.8)", cex=0.5)
text(27.5, 3.2, "(0.7)", cex=0.5)
text(32.5, 3.2, "(1.0)", cex=0.5)
text(3.5, 3.2, "(0.6)", cex=0.5) # Mar 15
text(8.5, 3.2, "(0.7)", cex=0.5)
text(13.5, 4.2, "(0.8)", cex=0.5)
text(18.5, 4.2, "(0.5)", cex=0.5)
text(23.5, 3.2, "(0.9)", cex=0.5)
text(28.5, 4.2, "(0.6)", cex=0.5)
text(33.5, 3.2, "(0.9)", cex=0.5)
text(4.5, 3.2, "(0.7)", cex=0.5) # Jun 16
text(9.5, 2.2, "(0.6)", cex=0.5)
text(14.5, 4.2, "(0.7)", cex=0.5)
text(19.5, 4.2, "(0.3)", cex=0.5)
text(24.5, 3.2, "(0.9)", cex=0.5)
text(29.5, 3.2, "(0.8)", cex=0.5)
text(34.5, 3.2, "(1.0)", cex=0.5)

# Pre- Post for June 2016
d.J16 <- rbind(J16.medpre, J16.medpost)
colnames(d.J16)[1:7] <- cols
barplot(as.matrix(d.J16), 
        ylab = "Median Score", ylim=c(0,4.9),
        main = "Pre- and Post-Surveys",
        cex.lab = 0.9,
        beside=TRUE, col=c("gray80", "gray 20") ,
        names.arg=c("Knowledge", 
                    "Skill", 
                    "Effective", 
                    "Motivated",
                    "Support\nOthers", 
                    "Support\nColleague",
                    "Evaluations"), cex.names = 0.9, las=1)
legend("topleft", 
       c("Pre-Survey","Post-Survey"), 
       cex=0.7, bty="n", fill=c("gray80", "gray 20"), ncol=2)
text(2.0, 4.3, "Note: SDs are above bars.", cex=0.6)
text(1.5, 3.2, "(0.8)", cex=0.6) # Pre
text(4.5, 2.2, "(0.8)", cex=0.6)
text(7.5, 4.2, "(1.1)", cex=0.6)
text(10.5, 4.2, "(0.8)", cex=0.6)
text(13.5, 4.2, "(0.9)", cex=0.6)
text(16.5, 3.2, "(1.2)", cex=0.6)
text(19.5, 3.2, "(1.2)", cex=0.6)
text(2.5, 3.2, "(0.7)", cex=0.6) # Post
text(5.5, 2.2, "(0.6)", cex=0.6)
text(8.5, 4.2, "(0.7)", cex=0.6)
text(11.5, 4.2, "(0.3)", cex=0.6)
text(14.5, 3.2, "(0.9)", cex=0.6)
text(17.5, 3.2, "(0.8)", cex=0.6)
text(20.5, 3.2, "(1.0)", cex=0.6)

# Normalized Mean Differences (Entire Sample)

# Create a matrix of normalized gains across workshops
cols <- c("Knowledge", "Skill", "Effective", "Motivated", "Support Others", 
          "Support Colleague", "Evaluations")
J15.ngain <- c(0.2, 0.1, 0.1, 0.1, NA, NA, NA)
N15.ngain <- c(0.1, 0.1, 0.0, NA, 0.0, 0.1, 0.1)
M16.ngain <- c(0.1, 0.1, 0.1, 0.1, 0.0, 0.0, 0.1)
J16.ngain <- c(0.1, 0.0, 0.1, 0.1, 0.0, 0.1, 0.0)
d.ngain <- rbind(J15.ngain, N15.ngain, M16.ngain, J16.ngain)  
colnames(d.ngain)[1:7] <- cols
barplot(as.matrix(d.ngain), 
        ylab = "Normalized Mean Gain", ylim=c(0,0.4),
        main = "Normalized Gain Score \n (Entire Sample)",
        cex.lab = 0.9,
        beside=TRUE, col=c("gray80", "gray60", "gray40", "gray 20") ,
        names.arg=c("Knowledge", 
                    "Skill", 
                    "Effective", 
                    "Motivated",
                    "Support\nOthers", 
                    "Support\nColleague",
                    "Evaluations"), cex.names = 0.9, las=1)
legend("topright", 
       c("Jun 2015","Nov 2015","Mar 2016", "Jun 2016"), 
       cex=0.55, bty="n", fill=c("gray80", "gray60", "gray40", "gray 20"), ncol=2)
text(9.5, 0.01, "0.0", cex=0.55)
text(12.5, 0.01, "0.0", cex=0.55)
text(17.5, 0.01, "NA", cex=0.55)
text(21.5, 0.01, "NA", cex=0.55)
text(22.5, 0.01, "0.0", cex=0.55)
text(23.5, 0.01, "0.0", cex=0.55)
text(24.5, 0.01, "0.0", cex=0.55)
text(26.5, 0.01, "NA", cex=0.55)
text(28.5, 0.01, "0.0", cex=0.55)
text(31.5, 0.01, "NA", cex=0.55)
text(34.5, 0.01, "0.0", cex=0.55)

