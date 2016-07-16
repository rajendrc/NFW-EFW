library(dplyr)
library(psych)
library(readxl)

#-----------------------------------------------------------------------------------
# Load Clean Data File
#-----------------------------------------------------------------------------------

setwd("~/Documents/Rajendra Chattergoon/CU Boulder/Consulting/Stephanie Chasteen/2015-16 NFW/1516 NFW Survey")
d <- read.csv("2015_1215_NFW_Survey_PrePost.csv")

#-----------------------------------------------------------------------------------
# Demographic Analysis
#-----------------------------------------------------------------------------------

d1 <- d[,c(4, 13:23, 27)]
d1[, c(1, 13)] <- sapply(d1[, c(1, 13)], as.character)
d1 <- mutate_each(d1, funs(toupper))
d1c <- d1[d1$Email == d1$Email_Post, ]

table(d1c$GENDER); table(d1c$GENDER)/62; (62-(sum(table(d1c$GENDER))))/62

describe(d1c[,3:8])
11/61; 3/61; 27/61; 1/61

table(d1c$UNDERGRAD); table(d1c$UNDERGRAD)/62; (62-(sum(table(d1c$UNDERGRAD))))/62
table(d1c$DOCTORAL); table(d1c$DOCTORAL)/62; (62-(sum(table(d1c$DOCTORAL))))/62

table(d1c$RESEARCH); table(d1c$RESEARCH)/62; (62-(sum(table(d1c$RESEARCH))))/62
table(d1c$SURVEY); table(d1c$SURVEY)/(sum(table(d1c$SURVEY)))

#-----------------------------------------------------------------------------------
# Q11-16 by gender
#-----------------------------------------------------------------------------------

d2 <- d[,c(4, 6:13, 27:33)]
d2[, c(2:9, 11:16)] <- sapply(d2[, c(2:9, 11:16)], as.numeric)
d2[, c(1, 10)] <- mutate_each(d2[, c(1, 10)], funs(toupper))
d2c <- d2[d2$Email == d2$Email_Post, ]
d2c[,4:8]<-d2c[,4:8]+1

table(d2c$PO_KNOWLEDGE); table(d2c$PO_KNOWLEDGE)/sum(table(d2c$PO_KNOWLEDGE))
table(d2c$PO_SKILL); table(d2c$PO_SKILL)/sum(table(d2c$PO_SKILL))

describe(d2c[,c(2:8, 11:16)]) 
describeBy(d2c[,c(2:8, 11:16)], d2c$GENDER) 

# Effect Sizes

(2.86-3.08)/0.69 # Knowledge Pre
(2.43-2.58)/0.75 # Skill Pre
(4.72-4.92)/0.42 # Effective Pre
(4.34-4.50)/0.77 # Supported by Others Pre
(4.03-4.00)/0.94 # Support a Colleague Pre
(3.66-4.33)/1.33 # Good Evaluations Pre

(3.31-3.73)/0.59 # Knowledge Post
(2.76-3.09)/0.53 # Skill Post
(4.59-4.82)/0.58 # Effective Post
(4.17-4.82)/0.83 # Supported by Others Post
(4.14-4.45)/0.66 # Support a Colleague Post
(4.03-4.36)/0.97 # Good Evaluations Post

#-----------------------------------------------------------------------------------
# Pre- Post-Gains
#-----------------------------------------------------------------------------------

d2c$G_KNOW <- d2c$PO_KNOWLEDGE - d2c$PR_KNOWLEDGE
d2c$G_SKIL <- d2c$PO_SKILL - d2c$PR_SKILL
d2c$G_EFFE <- d2c$PO_EFFECTIVE - d2c$PR_EFFECTIVE
d2c$G_SUPO <- d2c$PO_SUP_BY_OTHERS - d2c$PR_SUP_BY_OTHERS
d2c$G_SUPC <- d2c$PO_SUP_COLLEAGUE - d2c$PR_SUP_COLLEAGUE
d2c$G_EVAL <- d2c$PO_EVALUATIONS - d2c$PR_EVALUATIONS

G_QA <- c(0, table(d2c$G_KNOW))
G_QB <- c(0, table(d2c$G_SKIL))
G_QC <- c(table(d2c$G_EFFE), 0)
G_QD <- table(d2c$G_SUPO)
G_QE <- c(0, table(d2c$G_SUPC))
G_QF <- c("-4"=1, 0, 0, "-1"=6, "0"=23, "1"=5, "2"=1, "3"=2, "4"=2); table(d2c$G_EVAL)

QA.pr <- table(d2c$PR_KNOWLEDGE)
QB.pr <- table(d2c$PR_SKILL)
QC.pr <- c(0, 0, 0, table(d2c$PR_EFFECTIVE))
QD.pr <- c("1"=1, 0, "3"=1, "4"=19, "5"=20); table(d2c$PR_SUP_BY_OTHERS)
QE.pr <- c(0, table(d2c$PR_SUP_COLLEAGUE))
QF.pr <- c("1"=6, 0, "3"=3, "4"=17, "5"=15); table(d2c$PR_EVALUATIONS)

QA.po <- c(0, table(d2c$PO_KNOWLEDGE))
QB.po <- c(0, table(d2c$PO_SKILL))
QC.po <- c(0, 0, table(d2c$PO_EFFECTIVE))
QD.po <- c(0, table(d2c$PO_SUP_BY_OTHERS))
QE.po <- c(0, 0, table(d2c$PO_SUP_COLLEAGUE))
QF.po <- table(d2c$PO_EVALUATIONS)

describe(d2c[,c(2:8, 11:16)]) 
describe(d2c[,c(17:22)])
describeBy(d2c[,c(17:22)], d2c$GENDER)

# Gender Effect Sizes

(0.46-0.64)/0.82 # G_Knowledge
(0.32-0.45)/0.36 # G_Skill
(-0.14-(-0.09))/0.72 # G_Effectiveness
(-0.17-0.27)/0.81 # G_Support by Others
(0.10-0.27)/1.10 # G_Support Colleague
(0.38-0.00)/1.41 # G_Evaluation

# Normalized Bar Plot Gender

Knowledge <- c(0.1, 0.2)
Skill <- c(0.0, 0.1)
Effective <- c(0.1, 0.1)
Motivated <- c(0.1, 0.0)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
foo <- cbind(Knowledge, Skill, Effective, Motivated)
barplot(foo, 
        main="June 2015 Normalized Mean Differences",
        xlab="Common Question",
        ylab="Mean Difference",
        legend=c("Male","Female"),
        args.legend=list(x = "topright", bty="n"),
        beside=TRUE)

Know. <- c(0.1, 0.2)
Skill <- c(0.1, 0.1)
Effect. <- c(0.0, 0.0)
S_Oth. <- c(0.0, 0.1)
S_Col. <- c(0.0, 0.1)
Eval. <- c(0.1, 0.0)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
foo <- cbind(Know., Skill, Effect., S_Oth., S_Col., Eval.)
barplot(foo, 
        main="November 2015 Normalized Mean Differences",
        xlab="Common Question",
        ylab="Mean Difference",
        legend=c("Male","Female"),
        args.legend=list(x = "topright", bty="n"),
        beside=TRUE)


Know. <- c(0.1, 0.1)
Skill <- c(0.0, 0.1)
Effect. <- c(0.1, 0.0)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
male <- cbind(Know., Skill, Effect.)
barplot(male, 
        main="Normalized Gender Differences",
        ylim=c(0,0.2),
        xlab="Common Question",
        ylab="Mean Difference",
        legend=c("June 2015","November 2015"),
        args.legend=list(x = "topright", bty="n"),
        beside=TRUE)

Know. <- c(0.1, 0.1)
Skill <- c(0.0, 0.1)
Effect. <- c(0.1, 0.0)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
male <- cbind(Know., Skill, Effect.)
barplot(male, 
        main="Normalized Gender Differences",
        ylim=c(0,0.2),
        xlab="Common Question",
        ylab="Mean Difference",
        legend=c("June 2015","November 2015"),
        args.legend=list(x = "topright", bty="n"),
        beside=TRUE)

Know. <- c(0.2, 0.1)
Skill <- c(0.1, 0.1)
Effect. <- c(0.1, 0.0)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
norm <- cbind(Know., Skill, Effect.)
barplot(norm, 
        main="Normalized Mean Differences \n (Entire Sample)",
        xlab="Common Question",
        ylab="Mean Difference",
        legend=c("June 2015","November 2015"),
        args.legend=list(x = "topright", bty="n"),
        beside=TRUE)


Know. <- c(-0.1, -0.1)
Skill <- c(-0.1, -0.1)
Effect. <- c(0.0, 0.0)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
female <- cbind(Know., Skill, Effect.)
barplot(female, 
        main="Normalized Gender Gain",
        ylim=c(-0.10, 0.20),
        xlab="Common Question",
        ylab="Mean Difference",
        legend=c("June 2015","November 2015"),
        args.legend=list(x = "topright", bty="n"),
        beside=TRUE)


Know. <- c(0.2, 0.1)
Skill <- c(0.1, 0.1)
Effect. <- c(0.1, 0.0)
Mot. <- c(0.1, 0.0)
S_Oth. <- c(0.0, 0.0)
S_Col. <- c(0.0, 0.0)
Eval. <- c(0.0, 0.1)

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
foo <- cbind(Know., Skill, Effect., Mot., S_Oth., S_Col., Eval.)
barplot(foo, 
        main="Normalized Mean Differences \n (Entire Sample)",
        xlab="Common Question",
        ylab="Mean Difference",
        legend=c("June 2015","November 2015"),
        args.legend=list(x = "topright", bty="n"),
        beside=TRUE)



barplot(QA.pr, ylab="Frequency", ylim=c(0,60))
  text(4.4, 55, cex=0.9, "Mean = 2.9")
  text(4.4, 48, cex=0.9, "Median = 3")
  text(4.4, 41, cex=0.9, "    SD = 0.7")

barplot(QB.pr, ylab="Frequency", ylim=c(0,60))
  text(4.4, 55, cex=0.9, "Mean = 2.5")
  text(4.4, 48, cex=0.9, "Median = 3")
  text(4.4, 41, cex=0.9, "    SD = 0.8")

barplot(QC.pr, ylab="Frequency", ylim=c(0,60))
  text(5.5, 55, cex=0.9, "Mean = 4.8")
  text(5.5, 48, cex=0.9, "Median = 5")
  text(5.5, 41, cex=0.9, "    SD = 0.4")

barplot(QD.pr, ylab="Frequency", ylim=c(0,60))
  text(5.5, 55, cex=0.9, "Mean = 4.4")
  text(5.5, 48, cex=0.9, "Median = 4")
  text(5.5, 41, cex=0.9, "    SD = 0.8")
  
barplot(QE.pr, ylab="Frequency", ylim=c(0,60))
  text(5.5, 55, cex=0.9, "Mean = 4.0")
  text(5.5, 48, cex=0.9, "Median = 4")
  text(5.5, 41, cex=0.9, "    SD = 0.9")

barplot(QF.pr, ylab="Frequency", ylim=c(0,60))
  text(5.5, 55, cex=0.9, "Mean = 2.9")
  text(5.5, 48, cex=0.9, "Median = 3")
  text(5.5, 41, cex=0.9, "    SD = 0.7")
  
barplot(QA.po, ylab="Frequency", ylim=c(0,60))
  text(4.4, 55, cex=0.9, "Mean = 3.4")
  text(4.4, 48, cex=0.9, "Median = 3")
  text(4.4, 41, cex=0.9, "    SD = 0.6")
  
barplot(QB.po, ylab="Frequency", ylim=c(0,60))
  text(4.4, 55, cex=0.9, "Mean = 2.9")
  text(4.4, 48, cex=0.9, "Median = 3")
  text(4.4, 41, cex=0.9, "    SD = 0.5")
  
barplot(QC.po, ylab="Frequency", ylim=c(0,60))
  text(5.5, 55, cex=0.9, "Mean = 4.7")
  text(5.5, 48, cex=0.9, "Median = 5")
  text(5.5, 41, cex=0.9, "    SD = 0.6")

barplot(QD.po, ylab="Frequency", ylim=c(0,60))
  text(5.5, 55, cex=0.9, "Mean = 4.4")
  text(5.5, 48, cex=0.9, "Median = 5")
  text(5.5, 41, cex=0.9, "    SD = 0.8")

barplot(QE.po, ylab="Frequency", ylim=c(0,60))
  text(5.5, 55, cex=0.9, "Mean = 4.2")
  text(5.5, 48, cex=0.9, "Median = 5")
  text(5.5, 41, cex=0.9, "    SD = 0.7")

barplot(QF.po, ylab="Frequency", ylim=c(0,60))
  text(5.5, 55, cex=0.9, "Mean = 4.1")
  text(5.5, 48, cex=0.9, "Median = 5")
  text(5.5, 41, cex=0.9, "    SD = 1.0")

barplot(G_QA, ylab="Frequency", ylim=c(0,60))
  text(4.8, 55, cex=0.9, "Mean Difference = 0.5")
  text(4.8, 48, cex=0.9, "Norm. Difference = 0.1")
  text(4.8, 41, cex=0.9, "       Effect Size = 0.73")

barplot(G_QB, ylab="Frequency", ylim=c(0,60))
  text(4.8, 55, cex=0.9, "Mean Difference = 0.4")
  text(4.8, 48, cex=0.9, "Norm. Difference = 0.1")
  text(4.8, 41, cex=0.9, "       Effect Size = 0.55")
  
barplot(G_QC, ylab="Frequency", ylim=c(0,60))
  text(4.8, 55, cex=0.9, "Mean Difference = -0.2")
  text(4.8, 48, cex=0.9, "Norm. Difference = 0.0")
  text(4.8, 41, cex=0.9, "       Effect Size = -0.26")
  
barplot(G_QD, ylab="Frequency", ylim=c(0,60))
  text(4.8, 55, cex=0.9, " Mean Difference = 0.0")
  text(4.8, 48, cex=0.9, "Norm. Difference = 0.0")
  text(4.8, 41, cex=0.9, "       Effect Size = -0.05")
  
barplot(G_QE, ylab="Frequency", ylim=c(0,60))
  text(6.8, 55, cex=0.9, "Mean Difference = 0.2")
  text(6.8, 48, cex=0.9, "Norm. Difference = 0.0")
  text(6.8, 41, cex=0.9, "       Effect Size = 0.25")
  
barplot(G_QF, ylab="Frequency", ylim=c(0,60))
  text(8.8, 55, cex=0.9, "Mean Difference = 0.3")
  text(8.8, 48, cex=0.9, "Norm. Difference = 0.1")
  text(8.8, 41, cex=0.9, "       Effect Size = 0.23")

#-----------------------------------------------------------------------------------
# Exploratory Analyses
#-----------------------------------------------------------------------------------

PR <- read.csv("2015_1215_NFW_Survey_PrePost.csv")
PR <- read_excel("2015_1023_NFW_Survey.xls",
                 sheet=1,
                 col_names=FALSE)
PR <- PR[3:67,]  
cols = c(1, 14:21, 23:32, 34:43, 45:60, 62:89, 91:94, 96:132);    
PR[ ,cols] = apply(PR[, cols], 2, 
                   function(x) as.numeric(x))
rm(cols)  
  
PO <- read_excel("2015_1215_NFW_Survey.xls",
                  sheet=1,
                  col_names=FALSE)
PO <- PO[3:64,] # update lower bound with number of observations
cols = c(14:28, 30:50, 52:55, 60:65);    
PO[ ,cols] = apply(PO[, cols], 2, 
                    function(x) as.numeric(x))
rm(cols)

PR<-data.frame(lapply(PR, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
  }
  ))

PO<-data.frame(lapply(PO, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
  }
  ))

d3 <- merge(PR, PO, by="X13")
d3<-d3[,c(1, 14:21, 96:121, 143:197)]
d3[,5:9]<-d3[,5:9]+1

d3$G_KNOW <- d3$X60.y - d3$X15.x
d3$G_SKIL <- d3$X61.y - d3$X16.x
d3$G_EFFE <- d3$X62.y - d3$X17.x
d3$G_SUPO <- d3$X63.y - d3$X19.x
d3$G_SUPC <- d3$X64.y - d3$X20.x
d3$G_EVAL <- d3$X65.y - d3$X21.x

cor(d3[ ,c(11:34, 91:96)], use="complete.obs")

# Effect Sizes of More/Less Time Teaching Active Learning

Q17 <- c(0, table(d3$X121)); table(d3$X121)/sum(table(d3$X121))
describe(d3$X121)

barplot(Q17, ylab="Frequency", ylim=c(0,60))

d3$MORE<-0
d3$MORE <- ifelse(d3$X121>=4, d3$MORE<-1, 0)

describe(d3[,c(3:5, 7:9, 84:89)])
describeBy(d3[,c(3:5, 7:9, 84:89)], d3$MORE) 

(3.24-2.63)/0.69 # Knowledge Pre
(2.76-2.21)/0.75 # Skill Pre
(4.78-4.74)/0.42 # Effective Pre
(4.56-4.26)/0.77 # Supported by Others Pre
(4.44-3.58)/0.94 # Support a Colleague Pre
(4.06-3.58)/1.33 # Good Evaluations Pre

(3.56-3.33)/0.59 # Knowledge Post
(3.00-2.72)/0.53 # Skill Post
(4.83-4.44)/0.58 # Effective Post
(4.72-4.06)/0.83 # Supported by Others Post
(4.22-4.22)/0.66 # Support a Colleague Post
(4.28-4.06)/0.97 # Good Evaluations Post

# Teaching Experience

Q5 <- table(d3$X14.x); sum(table(d3$X14.x))
Q5 <-  c("0"=7, "1"=12, "2"=7, "3"=3, "4"=4, "5"=5, "6"=2, 0, 0, 0, 0, 0, "12"=1)
describe(d3$X14.x)

barplot(Q5, ylab="Frequency", ylim=c(0,20))

d3$C1<-0
d3$C1 <- ifelse(d3$X14.x>=1, d3$C1<-1, 0)

d3$C2<-0
d3$C2 <- ifelse(d3$X14.x>=2, d3$C2<-1, 0)

d3$C3<-0
d3$C3 <- ifelse(d3$X14.x>=4, d3$C3<-1, 0)

describe(d3[,c(3:5, 7:9, 84:89)])
describeBy(d3[,c(3:5, 7:9, 84:89)], d3$C1) 
describeBy(d3[,c(3:5, 7:9, 84:89)], d3$C2) 
describeBy(d3[,c(3:5, 7:9, 84:89)], d3$C3) 

# Category 1

(2.94-2.86)/0.69 # Knowledge Pre
(2.52-2.29)/0.75 # Skill Pre
(4.82-4.57)/0.42 # Effective Pre
(4.35-4.57)/0.77 # Supported by Others Pre
(4.00-4.14)/0.94 # Support a Colleague Pre
(3.88-3.71)/1.33 # Good Evaluations Pre

(3.42-3.43)/0.59 # Knowledge Post
(2.82-3.00)/0.53 # Skill Post
(4.67-4.57)/0.58 # Effective Post
(4.27-4.71)/0.83 # Supported by Others Post
(4.24-4.14)/0.66 # Support a Colleague Post
(4.09-4.29)/0.97 # Good Evaluations Post

# Category 2

(3.05-2.79)/0.69 # Knowledge Pre
(2.57-2.37)/0.75 # Skill Pre
(4.77-4.79)/0.42 # Effective Pre
(4.27-4.53)/0.77 # Supported by Others Pre
(4.05-4.00)/0.94 # Support a Colleague Pre
(3.59-4.16)/1.33 # Good Evaluations Pre

(3.48-3.37)/0.59 # Knowledge Post
(2.86-2.84)/0.53 # Skill Post
(4.67-4.63)/0.58 # Effective Post
(4.24-4.47)/0.83 # Supported by Others Post
(4.19-4.26)/0.66 # Support a Colleague Post
(3.95-4.32)/0.97 # Good Evaluations Post

# Category 3

(3.27-2.79)/0.69 # Knowledge Pre
(2.55-2.45)/0.75 # Skill Pre
(4.83-4.76)/0.42 # Effective Pre
(4.33-4.41)/0.77 # Supported by Others Pre
(3.92-4.07)/0.94 # Support a Colleague Pre
(3.75-3.90)/1.33 # Good Evaluations Pre

(3.64-3.34)/0.59 # Knowledge Post
(3.00-2.79)/0.53 # Skill Post
(4.82-4.59)/0.58 # Effective Post
(4.27-4.38)/0.83 # Supported by Others Post
(4.18-4.24)/0.66 # Support a Colleague Post
(4.00-4.17)/0.97 # Good Evaluations Post




############







PO$X13 <- mutate_each(PO$X13, funs(toupper))


d3 <- merge(PR, PO, by"")
d2[, c(2:9, 11:16)] <- sapply(d2[, c(2:9, 11:16)], as.numeric)
d2[, c(1, 10)] <- mutate_each(d2[, c(1, 10)], funs(toupper))
d2c <- d2[d2$Email == d2$Email_Post, ]
d2c[,4:8]<-d2c[,4:8]+1


  
  
  
  
  
  
  
  
  
d3 <- d[,c(4:13, 27:33)]
  
  
  
d2[, c(2:9, 11:16)] <- sapply(d2[, c(2:9, 11:16)], as.numeric)
d2[, c(1, 10)] <- mutate_each(d2[, c(1, 10)], funs(toupper))
d2c <- d2[d2$Email == d2$Email_Post, ]
d2c[,4:8]<-d2c[,4:8]+1
  
table(d2c$PO_KNOWLEDGE); table(d2c$PO_KNOWLEDGE)/sum(table(d2c$PO_KNOWLEDGE))
table(d2c$PO_SKILL); table(d2c$PO_SKILL)/sum(table(d2c$PO_SKILL))
  
describe(d2c[,c(2:8, 11:16)]) 
describeBy(d2c[,c(2:8, 11:16)], d2c$GENDER) 
  

  
  
  
  
    
  
  
  
barplot(X16, ylab="Frequency", ylim=c(0,60))


