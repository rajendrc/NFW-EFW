# Plot #1

Know. <- c(0.6, 0.5)
Skill <- c(0.1, 0.4)
Effect. <- c(0.6, -0.2)
Mot. <- c(0.3, 0.0)
S_Oth. <- c(0.0, 0.0)
S_Col. <- c(0.0, 0.2)
Eval. <- c(0.0, 0.3)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
foo <- cbind(Know., Skill, Effect., Mot., S_Oth., S_Col., Eval.)
barplot(foo, 
        main="Gains on Individual Questions \n in June and November",
        ylim=c(-0.2, 1.2),
        xlab="Common Question",
        ylab="Average Gain",
        legend=c("June 2015","November 2015"),
        args.legend=list(x = "topright", bty="n", cex=0.8),
        beside=TRUE)
text(7.5, 0.7, cex=1.0, "*")
text(2.5, 0.6, cex=1.0, "*")
text(5.5, 0.5, cex=1.0, "*")
text(11.5, 0.1, cex=0.7, "NA")
text(13.5, 0.1, cex=0.7, "NA")
text(14.5, 0.1, cex=0.7, "0.0")
text(16.5, 0.1, cex=0.7, "NA")
text(19.5, 0.1, cex=0.7, "NA")
text(19.3, 0.8, cex=0.8, "*    |Effect Size| > 0.5")

# Plot #2

Know. <- c(0.4, 0.9, 0.5, 0.6)
Skill <- c(0.0, 0.4, 0.3, 0.5)
Effect. <- c(0.7, 0.5, -0.1, -0.1)
Mot. <- c(0.5, 0.1, 0.0, 0.0)
S_Oth. <- c(0.0, 0.0, -0.2, 0.3)
S_Col. <- c(0.0, 0.0, 0.1, 0.3)
Eval. <- c(0.0, 0.0, 0.4, 0.0)


Know. <- c(0.4, 0.5, 0.9, 0.6)
Skill <- c(0.0, 0.3, 0.4, 0.5)
Effect. <- c(0.7, -0.1, 0.5, -0.1)
Mot. <- c(0.5, 0.0, 0.1, 0.0)
S_Oth. <- c(0.0, -0.2, 0.0, 0.3)
S_Col. <- c(0.0, 0.1, 0.0, 0.3)
Eval. <- c(0.0, 0.4, 0.0, 0.0)


par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
foo <- cbind(Know., Skill, Effect., Mot., S_Oth., S_Col., Eval.)
barplot(foo, 
        main="Gender Differences in Gains on Individual Questions \n in June and November ",
        ylim=c(-0.2, 1.2),
        xlab="Common Question",
        ylab="Gain",
        legend=c("Male June 2015"," Male November 2015", "Female June 2015", "Female November 2015"),
        col=c("navy", "gray30", "blue", "gray80"),
        args.legend=list(x = "topright", bty="n", cex=0.8),
        beside=TRUE)
text(6.5, 0.05, cex=0.5, "0.0")
text(17.5, 0.05, cex=0.5, "NA")
text(19.5, 0.05, cex=0.5, "NA")
text(21.5, 0.05, cex=0.5, "NA")
text(23.5, 0.05, cex=0.5, "NA")
text(26.5, 0.05, cex=0.5, "NA")
text(28.5, 0.05, cex=0.5, "NA")
text(31.5, 0.05, cex=0.5, "NA")
text(33.5, 0.05, cex=0.5, "NA")
text(34.5, 0.05, cex=0.5, "0.0")

# Plot 3


Know. <- c(0.4, 0.5)
Skill <- c(0.0, 0.3)
Effect. <- c(0.7, -0.1)
Mot. <- c(0.5, 0.0)
S_Oth. <- c(0.0, -0.2)
S_Col. <- c(0.0, 0.1)
Eval. <- c(0.0, 0.4)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
foo <- cbind(Know., Skill, Effect., Mot., S_Oth., S_Col., Eval.)
barplot(foo, 
        main="Male Differences in Gains on Individual Questions \n in June and November ",
        ylim=c(-0.2, 1.2),
        xlab="Common Question",
        ylab="Gain",
        legend=c("Male June 2015"," Male November 2015"),
        args.legend=list(x = "topright", bty="n", cex=0.8),
        beside=TRUE)

text(4.5, 0.1, cex=0.8, "0.0")
text(11.5, 0.1, cex=0.8, "NA")
text(13.5, 0.1, cex=0.8, "NA")
text(16.5, 0.1, cex=0.8, "NA")
text(19.5, 0.1, cex=0.8, "NA")



Know. <- c(0.9, 0.6)
Skill <- c(0.4, 0.5)
Effect. <- c(0.5, -0.1)
Mot. <- c(0.1, 0.0)
S_Oth. <- c(0.0, 0.3)
S_Col. <- c(0.0, 0.3)
Eval. <- c(0.0, 0.0)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
foo <- cbind(Know., Skill, Effect., Mot., S_Oth., S_Col., Eval.)
barplot(foo, 
        main="Female Differences in Gains on Individual Questions \n in June and November ",
        ylim=c(-0.2, 1.2),
        xlab="Common Question",
        ylab="Gain",
        legend=c("Female June 2015", "Female November 2015"),
        args.legend=list(x = "topright", bty="n", cex=0.8),
        beside=TRUE)
text(11.5, 0.1, cex=0.8, "NA")
text(13.5, 0.1, cex=0.8, "NA")
text(16.5, 0.1, cex=0.8, "NA")
text(19.5, 0.1, cex=0.8, "NA")
text(20.5, 0.1, cex=0.8, "0.0")

