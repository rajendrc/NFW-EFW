#-----------------------------------------------------------------------------------
# Load Clean Data File
#-----------------------------------------------------------------------------------

NFW <- read_excel("2015_1023_NFW_Survey.xls",
                      sheet=1,
                      col_names=FALSE)
NFW <- NFW[3:67,] # update lower bound with number of observations
cols = c(1, 14:21, 23:32, 34:43, 45:60, 62:89, 91:94, 96:132);    
NFW[ ,cols] = apply(NFW[, cols], 2, 
                    function(x) as.numeric(x))
rm(cols)

#-----------------------------------------------------------------------------------
# Analysis - Section 4
#-----------------------------------------------------------------------------------

library(psych)
library(ltm)
library(sjPlot)

table(NFW$X96);table(NFW$X96)/sum(table(NFW$X96))
describe(NFW[,96])

# construct 1

P02 <- table(NFW$X98); table(NFW$X98)/sum(table(NFW$X98))
P04 <- table(NFW$X100); table(NFW$X100)/sum(table(NFW$X100))
P06 <- table(NFW$X102); table(NFW$X102)/sum(table(NFW$X102))
P07 <- table(NFW$X103); table(NFW$X103)/sum(table(NFW$X103))
P08 <- table(NFW$X104); table(NFW$X104)/sum(table(NFW$X104))
P09 <- table(NFW$X105); table(NFW$X105)/sum(table(NFW$X105))
P10 <- table(NFW$X106); table(NFW$X106)/sum(table(NFW$X106))
P12 <- table(NFW$X108); table(NFW$X108)/sum(table(NFW$X108))
P13 <- table(NFW$X109); table(NFW$X109)/sum(table(NFW$X109))
P14 <- table(NFW$X110); table(NFW$X110)/sum(table(NFW$X110))
P15 <- table(NFW$X111); table(NFW$X111)/sum(table(NFW$X111))
P16 <- table(NFW$X112); table(NFW$X112)/sum(table(NFW$X112))
P18 <- table(NFW$X114); table(NFW$X114)/sum(table(NFW$X114))
P19 <- table(NFW$X115); table(NFW$X115)/sum(table(NFW$X115))
P20 <- table(NFW$X116); table(NFW$X116)/sum(table(NFW$X116))

barplot(P02, ylab="Frequency", ylim=c(0,60))
barplot(P04, ylab="Frequency", ylim=c(0,60))
barplot(P06, ylab="Frequency", ylim=c(0,60))
barplot(P07, ylab="Frequency", ylim=c(0,60))
barplot(P08, ylab="Frequency", ylim=c(0,60))
barplot(P09, ylab="Frequency", ylim=c(0,60))
barplot(P10, ylab="Frequency", ylim=c(0,60))
barplot(P12, ylab="Frequency", ylim=c(0,60))
barplot(P13, ylab="Frequency", ylim=c(0,60))
barplot(P14, ylab="Frequency", ylim=c(0,60))
barplot(P15, ylab="Frequency", ylim=c(0,60))
barplot(P16, ylab="Frequency", ylim=c(0,60))
barplot(P18, ylab="Frequency", ylim=c(0,60))
barplot(P19, ylab="Frequency", ylim=c(0,60))
barplot(P20, ylab="Frequency", ylim=c(0,60))

describe(NFW[, c(98, 100, 102:106, 108:112, 114:116)])
descript(NFW[, c(98, 100, 102:106, 108:112, 114:116)])  # single scale

NFW$C1 <- NFW$X98 + NFW$X100 + NFW$X102 + NFW$X103 + NFW$X104 + NFW$X105 +
  NFW$X106 + NFW$X108 + NFW$X109 + NFW$X110 + NFW$X111 + NFW$X112 + NFW$X114 +
  NFW$X115 + NFW$X116
NFW$C1 <- (NFW$C1/75)*100

par(mar=c(5,5,5,2))
hist(NFW$C1, main="Frequency Distribution of Raw Scores for \n Student-Centered Practice Construct", 
        xlab="Total Score \n (Percent out of 75 Possible Points)",
        xlim=c(0,100),
        ylim=c(0,30),
        col="grey")

# sjp.likert(NFW[, c(98, 100, 102:106, 108:112, 114:116)], 
#           catcount = 4, 
#           cat.neutral=3,
#           legendLabels=c("Not at all", "Minimally", "Mostly", "Very", "Somewhat"),
#           value.labels="sum.outside",
#           geom.colors="Blues",
#           cat.neutral.color = "Grey",
#           showPercentageSign=TRUE,
#           expand.grid=TRUE,
#           includeN=TRUE,
#           gridRange = 1.5,
#           axisLabels.y=c(
#             "15.b. I design activities that connect course content to my students’ lives and future work.",
#             "15.d. I provide students with immediate feedback on their work during class (e.g., student response systems, short quizzes, etc.).",
#             "15.f. I use student assessment results to guide the direction of my instruction during the semester.",
#             "15.g. I frequently ask students to respond to questions during class time.",
#             "15.h. I use student questions and comments to determine the focus and direction of classroom discussion.",
#             "15.i. I have students use a variety of means (models, drawings, graphs, symbols, simulations, etc.) to represent phenomena.",
#             "15.j. I structure class so that students explore or discuss their understanding of new concepts before formal instruction.",
#             "16.a. I structure class so that students regularly talk with one another about course concepts.",
#             "16.b. I structure class so that students constructively criticize one another’s ideas.",
#             "16.c. I structure class so that students discuss the difficulties they have with this subject with other students.",
#             "16.d. I require students to work together in small groups.",
#             "16.e. I structure problems so that students consider multiple approaches to finding a solution.", 
#             "16.g. I give students frequent assignments worth a small portion of their grade.", 
#             "16.h. I require students to make connections between all related ideas or concepts when completing assignments.", 
#             "16.i. I provide feedback on student assignments without assigning a formal grade."),
#           title="Frequency Distribution of Responses for Student-Centered Practice Construct"
#) 

# for p-values:

# NFW[, c(98, 100, 102:106, 108:112, 114:116)]<-NFW[, c(98, 100, 102:106, 108:112, 114:116)]/5
# describe(NFW[, c(98, 100, 102:106, 108:112, 114:116)])

# construct 2

P01 <- table(NFW$X97); table(NFW$X97)/sum(table(NFW$X97))
P03 <- table(NFW$X99); table(NFW$X99)/sum(table(NFW$X99))
P05 <- table(NFW$X101); table(NFW$X101)/sum(table(NFW$X101))
P11 <- table(NFW$X107); table(NFW$X107)/sum(table(NFW$X107))
P17 <- table(NFW$X113); table(NFW$X113)/sum(table(NFW$X113))
P21 <- table(NFW$X117); table(NFW$X117)/sum(table(NFW$X117))
P22 <- table(NFW$X118); table(NFW$X118)/sum(table(NFW$X118))
P23 <- table(NFW$X119); table(NFW$X119)/sum(table(NFW$X119))
P24 <- table(NFW$X120); table(NFW$X120)/sum(table(NFW$X120))

barplot(P01, ylab="Frequency", ylim=c(0,60))
barplot(P03, ylab="Frequency", ylim=c(0,60))
barplot(P05, ylab="Frequency", ylim=c(0,60))
barplot(P11, ylab="Frequency", ylim=c(0,60))
barplot(P17, ylab="Frequency", ylim=c(0,60))
barplot(P21, ylab="Frequency", ylim=c(0,60))
barplot(P22, ylab="Frequency", ylim=c(0,60))
barplot(P23, ylab="Frequency", ylim=c(0,60))
barplot(P24, ylab="Frequency", ylim=c(0,60))

describe(NFW[, c(97, 99, 101, 107, 113, 117:120)])
descript(NFW[, c(97, 99, 101, 107, 113, 117:120)])

NFW$C2 <- NFW$X97 + NFW$X99 + NFW$X101 + NFW$X113 + NFW$X117 + NFW$X118 +
  NFW$X119 + NFW$X20
NFW$C2 <- (NFW$C2/45)*100

par(mar=c(5,5,5,2))
hist(NFW$C2, main="Frequency Distribution of Raw Scores for \n Instructor-Centered Practice Construct", 
     breaks=5,
     xlab="Total Score \n (Percent out of 75 Possible Points)",
     xlim=c(0,100),
     ylim=c(0,30),
     col="grey")

# sjp.likert(NFW[, c(97, 99, 101, 107, 113, 117:120)], 
#           catcount = 4, 
#           cat.neutral=3,
#           legendLabels=c("Not at all", "Minimally", "Mostly", "Very", "Somewhat"),
#           value.labels="sum.outside",
#           geom.colors="Blues",
#           cat.neutral.color = "Grey",
#           showPercentageSign=TRUE,
#           expand.grid=TRUE,
#           includeN=TRUE,
#           gridRange = 1.5,
#           axisLabels.y=c(
#             "15.a. I guide students through major topics as they listen and take notes.",
#             "15.c. My syllabus contains the specific topics that will be covered in every class session.", 
#             "15.e. I structure my course with the assumption that most of the students have little knowledge of the topics.",
#             "15.k. My class sessions are structured to give students a good set of notes.", 
#             "16.f. I provide time for students to reflect about the process they use to solve problems.", 
#             "16.j. My test questions focus on important facts and definitions from the course.", 
#             "16.k. My test questions require students to apply course concepts to unfamiliar situations.", 
#             "16.l. My test questions contain well-defined problems with one correct solution.", 
#             "16.m. I adjust student scores (e.g., curve) when necessary to reflect a proper distribution of grades."),
#           title="Frequency Distribution of Responses for Instructor-Centered Practice Construct"
#) 

# for p-values

# NFW[, c(97, 99, 101, 107, 113, 117:120)]<-NFW[, c(97, 99, 101, 107, 113, 117:120)]/5
# describe(NFW[, c(97, 99, 101, 107, 113, 117:120)])

Q17 <- table(NFW$X121); table(NFW$X121)/sum(table(NFW$X121))
describe(NFW$X121)

barplot(Q17, ylab="Frequency", ylim=c(0,60))


# sjp.likert(NFW$X121, 
#           catcount = 4, 
#           cat.neutral=1,
#           legendLabels=c("10% or less", "15-25%", "30-50%", "More than 50%", "None"),
#           value.labels="show",
#           geom.colors="Blues",
#           cat.neutral.color = "Grey",
#           showPercentageSign=TRUE,
#           includeN=TRUE,
#           axisLabels.y="Approximately how much class time do you currently dedicate to teaching using active-learning strategies?",
#           title="Question 17"
#) 

describe(NFW[,133:134])
