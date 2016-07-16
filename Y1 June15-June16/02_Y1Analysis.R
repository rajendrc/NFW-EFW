setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/Year 1")
source("00_MergedY1Cohort.R")

table(Y1$Know.Pre); table(Y1$Know.Post)
table(Y1$Skill.Pre); table(Y1$Skill.Post)
table(Y1$Eff.Pre); table(Y1$Eff.Post)
table(Y1$Mot.Pre); table(Y1$Mot.Post)
table(Y1$SuppOth.Pre); table(Y1$SuppOth.Post)
table(Y1$SuppCol.Pre); table(Y1$SuppCol.Post)
table(Y1$Eval.Pre); table(Y1$Eval.Post)                             

# Generate InExper and UgradinNA Variable
Y1$InExper <- ifelse(Y1$TchExp<3, 1, 0)
Y1$UgradinNA <- ifelse(Y1$Ugrad==6, 1, 0)

# Pre- Post- Descriptives with Gains

I1 <- Y1[is.na(Y1$Know.Gain)==FALSE,c(1,3,10,17,22,25,32:33)]
I2 <- Y1[is.na(Y1$Skill.Gain)==FALSE,c(1,4,11,17,22,26,32:33)]
I3 <- Y1[is.na(Y1$Eff.Gain)==FALSE,c(1,5,12,17,22,27,32:33)]
I4 <- Y1[is.na(Y1$Mot.Gain)==FALSE,c(1,6,13,17,22,28,32:33)]
I5 <- Y1[is.na(Y1$SuppOth.Gain)==FALSE,c(1,7,14,17,22,29,32:33)]
I6 <- Y1[is.na(Y1$SuppCol.Gain)==FALSE,c(1,8,15,17,22,30,32:33)]
I7 <- Y1[is.na(Y1$Eval.Gain)==FALSE,c(1,9,16,17,22,31,32:33)]

describe(I1[,2:4]); sd(as.numeric(as.matrix(I1[,2:3])))
   (3.28-2.80)/0.7264963
describe(I2[,2:4]); sd(as.numeric(as.matrix(I2[,2:3])))
   (2.64-2.40)/0.713366
describe(I3[,2:4]); sd(as.numeric(as.matrix(I3[,2:3])))
   (3.57-3.56)/0.6288884
describe(I4[,2:4]); sd(as.numeric(as.matrix(I4[,2:3])))
   (3.52-3.51)/0.6278783
describe(I5[,2:4]); sd(as.numeric(as.matrix(I5[,2:3])))
   (3.07-3.35)/0.815443
describe(I6[,2:4]); sd(as.numeric(as.matrix(I6[,2:3])))
   (3.04-3.05)/0.866811  
describe(I7[,2:4]); sd(as.numeric(as.matrix(I7[,2:3])))
   (2.93-3.18)/0.7842666

# Pre- Post- Gain Distributions

barplot(table(I1[,2]), ylim=c(0,100))
barplot(table(I1[,3]), ylim=c(0,100))
barplot(table(I1[,4]), ylim=c(0,100))

barplot(table(I2[,2]), ylim=c(0,100))
barplot(table(I2[,3]), ylim=c(0,100))
barplot(table(I2[,4]), ylim=c(0,100))

barplot(table(I3[,2]), ylim=c(0,100))
barplot(c(0, table(I3[,3])), ylim=c(0,100))
barplot(table(I3[,4]), ylim=c(0,100))

barplot(c(0,table(I4[,2])), ylim=c(0,100))
barplot(c(0,table(I4[,3])), ylim=c(0,100))
barplot(table(I4[,4]), ylim=c(0,100))

barplot(table(I5[,2]), ylim=c(0,100))
barplot(table(I5[,3]), ylim=c(0,100))
barplot(table(I5[,4]), ylim=c(0,100))

barplot(table(I6[,2]), ylim=c(0,100))
barplot(table(I6[,3]), ylim=c(0,100))
barplot(table(I6[,4]), ylim=c(0,100))

barplot(table(I7[,2]), ylim=c(0,100))
barplot(table(I7[,3]), ylim=c(0,100))
barplot(table(I7[,4]), ylim=c(0,100))

# Gender Analysis

describeBy(I1[,c(2:3,5)], group=I1$Gender)
describeBy(I2[,c(2:3,5)], group=I2$Gender)
describeBy(I3[,c(2:3,5)], group=I3$Gender)
describeBy(I4[,c(2:3,5)], group=I4$Gender)
describeBy(I5[,c(2:3,5)], group=I5$Gender)
describeBy(I6[,c(2:3,5)], group=I6$Gender)
describeBy(I7[,c(2:3,5)], group=I7$Gender)

# Percent of Active Learning

describeBy(I1[,c(2:3,6)], group=I1$ClassTime)
describeBy(I2[,c(2:3,6)], group=I2$ClassTime)
describeBy(I3[,c(2:3,6)], group=I3$ClassTime)
describeBy(I4[,c(2:3,6)], group=I4$ClassTime)
describeBy(I5[,c(2:3,6)], group=I5$ClassTime)
describeBy(I6[,c(2:3,6)], group=I6$ClassTime)
describeBy(I7[,c(2:3,6)], group=I7$ClassTime)

# Teaching Experience 
describeBy(I1[,c(2:3,6)], group=I1$InExper)
describeBy(I2[,c(2:3,6)], group=I2$InExper)
describeBy(I3[,c(2:3,6)], group=I3$InExper)
describeBy(I4[,c(2:3,6)], group=I4$InExper)
describeBy(I5[,c(2:3,6)], group=I5$InExper)
describeBy(I6[,c(2:3,6)], group=I6$InExper)
describeBy(I7[,c(2:3,6)], group=I7$InExper)

# Ugrad in American 
describeBy(I1[,c(2:3,6)], group=I1$UgradinNA)
describeBy(I2[,c(2:3,6)], group=I2$UgradinNA)
describeBy(I3[,c(2:3,6)], group=I3$UgradinNA)
describeBy(I4[,c(2:3,6)], group=I4$UgradinNA)
describeBy(I5[,c(2:3,6)], group=I5$UgradinNA)
describeBy(I6[,c(2:3,6)], group=I6$UgradinNA)
describeBy(I7[,c(2:3,6)], group=I7$UgradinNA)

# Distributions of Ratings

Y1.b <- Y1.b[Y1.b$Cohort!="June2015",]
Y1.b[,17:18] <- 5-Y1.b[,17:18]
barplot(c(0,table(Y1.b$TheOverallQualityoftheWorkshopExceeded...)), ylim=c(0,80))
barplot(c(0,table(Y1.b$`I gained a broad perspective...`)), ylim=c(0,80))
describe(Y1.b[,17:18])

# Correlations

# Convert any NAs to Os
Y1[,c(3:16)][is.na(Y1[,c(3:16)])] <- 0

# Create a new variable for the total score for pre- and post-survey
Y1$PreTotal <- Y1$Know.Pre + Y1$Skill.Pre + Y1$Eff.Pre + Y1$Mot.Pre + Y1$SuppOth.Pre +
  Y1$SuppCol.Pre + Y1$Eval.Pre
Y1$PreTotal <- (Y1$PreTotal/28)*100
Y1$PostTotal <- Y1$Know.Post + Y1$Skill.Post + Y1$Eff.Post + Y1$Mot.Post + Y1$SuppOth.Post +
  Y1$SuppCol.Post + Y1$Eval.Post
Y1$PostTotal <- (Y1$PostTotal/28)*100

cor(Y1$PreTotal,Y1$PostTotal)
plot(Y1$PreTotal,Y1$PostTotal,
     ylim=c(0,100), ylab="Post Total Score",
     xlim=c(0,100), xlab="Pre Total Score")
text (90,20, "r=0.66")

par(mfrow=c(1,2))
cor(Y1$PIPS.C1,Y1$PreTotal, use="complete.obs")
plot(Y1$PIPS.C1,Y1$PreTotal,
     ylim=c(0,100), ylab="PIPS Student-Centered Total Score",
     xlim=c(0,100), xlab="Pre Total Score")
text (90,10, "r = 0.35")

cor(Y1$PIPS.C2,Y1$PreTotal, use="complete.obs")
plot(Y1$PIPS.C2,Y1$PreTotal,
     ylim=c(0,100), ylab="PIPS Teacher-Centered Total Score",
     xlim=c(0,100), xlab="Pre Total Score")
text (90,10, "r = -0.08")

cor(Y1$PIPS.C1,Y1$PostTotal, use="complete.obs")
plot(Y1$PIPS.C1,Y1$PostTotal,
     ylim=c(0,100), ylab="PIPS Student-Centered Total Score",
     xlim=c(0,100), xlab="Post Total Score")
text (90,10, "r = 0.22")

cor(Y1$PIPS.C2,Y1$PostTotal, use="complete.obs")
plot(Y1$PIPS.C2,Y1$PostTotal,
     ylim=c(0,100), ylab="PIPS Teacher-Centered Total Score",
     xlim=c(0,100), xlab="Post Total Score")
text (90,10, "r = -0.03")



