#-----------------------------------------------------------------------------------
# Load Clean Data File
#-----------------------------------------------------------------------------------

setwd("~/Documents/Rajendra Chattergoon/CU Boulder/Consulting/Stephanie Chasteen/2015-16 NFW/1516 NFW Survey")
source("00_Raw_Data.R")

#-----------------------------------------------------------------------------------
# Analysis Question 5
#-----------------------------------------------------------------------------------

library(sjPlot)
library(psych)

# Recoding 1=4, 2=3, 3=2, 4=1 
# Reverse coding so Strongly agree=4

NFW[,2:16]<-ifelse(NFW[,2:16] == 1, 10, 
                   ifelse(NFW[,2:16] == 2, 20, 
                          ifelse(NFW[,2:16] == 3, 30, 
                                 ifelse(NFW[,2:16] == 4, 40, 999
                                        )
                          )
                   )
)

NFW[,2:16]<-ifelse(NFW[,2:16] == 10, 4, 
                   ifelse(NFW[,2:16] == 20, 3, 
                          ifelse(NFW[,2:16] == 30, 2, 
                                 ifelse(NFW[,2:16] == 40, 1, 999
                                 )
                          )
                   )
)

# Frequencies of each response

X14 <- c("1"==0, table(NFW$X14)); table(NFW$X14)/sum(table(NFW$X14))
X15 <- c("1"==0, table(NFW$X15)); table(NFW$X15)/sum(table(NFW$X15))
X16 <- table(NFW$X16); table(NFW$X16)/sum(table(NFW$X16))
X17 <- table(NFW$X17); table(NFW$X17)/sum(table(NFW$X17))
X18 <- table(NFW$X18); table(NFW$X18)/sum(table(NFW$X18))
X19 <- c("1"==0, table(NFW$X19)); table(NFW$X19)/sum(table(NFW$X19))
X20 <- c("1"==0, table(NFW$X20)); table(NFW$X20)/sum(table(NFW$X20))
X21 <- table(NFW$X21); table(NFW$X21)/sum(table(NFW$X21))
X22 <- table(NFW$X22); table(NFW$X22)/sum(table(NFW$X22))
X23 <- table(NFW$X23); table(NFW$X23)/sum(table(NFW$X23))
X24 <- table(NFW$X24); table(NFW$X24)/sum(table(NFW$X24))
X25 <- table(NFW$X25); table(NFW$X25)/sum(table(NFW$X25))
X26 <- table(NFW$X26); table(NFW$X26)/sum(table(NFW$X26))
X27 <- table(NFW$X27); table(NFW$X27)/sum(table(NFW$X27))
X28 <- c("1"==0, table(NFW$X28)); table(NFW$X28)/sum(table(NFW$X28))

# Bar plots

barplot(X14, ylab="Frequency", ylim=c(0,60))
barplot(X15, ylab="Frequency", ylim=c(0,60))
barplot(X16, ylab="Frequency", ylim=c(0,60))
barplot(X17, ylab="Frequency", ylim=c(0,60))
barplot(X18, ylab="Frequency", ylim=c(0,60))
barplot(X19, ylab="Frequency", ylim=c(0,60))
barplot(X20, ylab="Frequency", ylim=c(0,60))
barplot(X21, ylab="Frequency", ylim=c(0,60))
barplot(X22, ylab="Frequency", ylim=c(0,60))
barplot(X23, ylab="Frequency", ylim=c(0,60))
barplot(X24, ylab="Frequency", ylim=c(0,60))
barplot(X25, ylab="Frequency", ylim=c(0,60))
barplot(X26, ylab="Frequency", ylim=c(0,60))
barplot(X27, ylab="Frequency", ylim=c(0,60))
barplot(X28, ylab="Frequency", ylim=c(0,60))

# For n, mean, median, sd, and p-values 

describe(NFW[,2:16])

NFW[,2:16]<-NFW[,2:16]/4
describe(NFW[,2:16])

# Likert bar plots:

sjp.setTheme(theme="theme_bw", title.align="center",
             title.size=1.8,
             axis.title.size=1.5,
             geom.label.size=5,
             legend.size=1,
             axis.textcolor.x="Black",
             axis.textcolor.y="Black")

sjp.likert(NFW[,2:16], 
           catcount = 4, 
           legendLabels=c("Strongly Agree",
                          "Agree", 
                          "Disagree",
                          "Strongly Disagree"),
           value.labels="sum.outside",
           geom.colors="Greys",
           showPercentageSign=TRUE,
           reverse.colors=TRUE,
           expand.grid=TRUE,
           sort.frq = "pos.desc",
           includeN=TRUE,
           axisLabels.y=c(
             "5.a. The overall quality of the workshop exceeded that of other professional development workshops I have attended.",
             "5.b. I gained a broad perspective of teaching techniques through the diversity of topics presented.",
             "5.c. I would have preferred fewer topics with more time per topic.",
             "5.d. I had adequate time to digest and process information presented.", 
             "5.e. The meeting schedule was too exhausting.",
             "5.f. I have a good sense of what many of these teaching techniques look like in the classroom (including relevant classroom examples).",
             "5.g. Meeting sessions gave adequate opportunities for active learning (through discussions, trying out techniques, etc.).",
             "5.h. Meeting sessions relied too much on presentation and lecture.",
             "5.i. I would have liked more opportunity to practice what I learned at the conference.",
             "5.j. I felt that presenters spent too much time explaining or justifying why their technique was effective.",
             "5.k. Presenters indicated that educational techniques were adaptable to a variety of situations.",
             "5.l. I had enough time to interact with my faculty colleagues.",
             "5.m. The workshops built on my existing interests and teaching experience.",
             "5.n. Information provided in advance of the workshop (e.g., emails, articles, schedule) was adequate for my preparation.",
             "5.o. Information and materials provided at the workshop (e.g., USB drive, handouts) were adequate for my future reference."
           ),
           gridRange=1.1,
           title="Question 5 \n Frequency Distribution of Responses"
) 

#-----------------------------------------------------------------------------------
# Analysis Question 6
#-----------------------------------------------------------------------------------

# Frequencies of each response (including 0 response)

X30 <- table(NFW$X30); table(NFW$X30)/sum(table(NFW$X30)); X30 <- c("0"=8, 0, 0, "3"=5, "4"=22, "5"=21)
X31 <- table(NFW$X31); table(NFW$X31)/sum(table(NFW$X31))
X32 <- c(0, 0, table(NFW$X32)); table(NFW$X32)/sum(table(NFW$X32))
X33 <- table(NFW$X33); table(NFW$X33)/sum(table(NFW$X33)); X33 <- c("0"=3, 0, "2"=1, "3"=6, "4"=19, "5"=27)
X34 <- table(NFW$X34); table(NFW$X34)/sum(table(NFW$X34))
X35 <- c(0, table(NFW$X35)); table(NFW$X35)/sum(table(NFW$X35))
X36 <- table(NFW$X36); table(NFW$X36)/sum(table(NFW$X36)); X36 <- c("0"=1, 0, "2"=1, "3"=9, "4"=30, "5"=15)
X37 <- table(NFW$X37); table(NFW$X37)/sum(table(NFW$X37))
X38 <- table(NFW$X38); table(NFW$X38)/sum(table(NFW$X38)); X38 <- c("0"=3, 0, "2"=4, "3"=7, "4"=25, "5"=17)
X39 <- table(NFW$X39); table(NFW$X39)/sum(table(NFW$X39)); X39 <- c("0"=37, 0, "2"=1, "3"=3, "4"=6, "5"=8)
X40 <- table(NFW$X40); table(NFW$X40)/sum(table(NFW$X40))
X41 <- table(NFW$X41); table(NFW$X41)/sum(table(NFW$X41))
X42 <- table(NFW$X42); table(NFW$X42)/sum(table(NFW$X42))
X43 <- table(NFW$X43); table(NFW$X43)/sum(table(NFW$X43))
X44 <- table(NFW$X44); table(NFW$X44)/sum(table(NFW$X44))
X45 <- table(NFW$X45); table(NFW$X45)/sum(table(NFW$X45))
X46 <- table(NFW$X46); table(NFW$X46)/sum(table(NFW$X46))
X47 <- table(NFW$X47); table(NFW$X47)/sum(table(NFW$X47)); X47 <- c("0"=5, 0, "2"=2, "3"=11, "4"=17, "5"=21)
X48 <- table(NFW$X48); table(NFW$X48)/sum(table(NFW$X48))
X49 <- table(NFW$X49); table(NFW$X49)/sum(table(NFW$X49))
X50 <- table(NFW$X50); table(NFW$X50)/sum(table(NFW$X50))

# Recoding 0=NA

NFW[,17:37][NFW[,17:37]==0] <- NA  

X30 <- c(0, 0, table(NFW$X30)); table(NFW$X30)/sum(table(NFW$X30))
X31 <- table(NFW$X31); table(NFW$X31)/sum(table(NFW$X31))
X32 <- c(0, table(NFW$X32)); table(NFW$X32)/sum(table(NFW$X32))
X33 <- c(0, table(NFW$X33)); table(NFW$X33)/sum(table(NFW$X33))
X34 <- table(NFW$X34); table(NFW$X34)/sum(table(NFW$X34))
X35 <- table(NFW$X35); table(NFW$X35)/sum(table(NFW$X35))
X36 <- c(0, table(NFW$X36)); table(NFW$X36)/sum(table(NFW$X36))
X37 <- table(NFW$X37); table(NFW$X37)/sum(table(NFW$X37))
X38 <- c(0, table(NFW$X38)); table(NFW$X38)/sum(table(NFW$X38))
X39 <- c(0, table(NFW$X39)); table(NFW$X39)/sum(table(NFW$X39))
X40 <- table(NFW$X40); table(NFW$X40)/sum(table(NFW$X40))
X41 <- table(NFW$X41); table(NFW$X41)/sum(table(NFW$X41))
X42 <- table(NFW$X42); table(NFW$X42)/sum(table(NFW$X42))
X43 <- table(NFW$X43); table(NFW$X43)/sum(table(NFW$X43))
X44 <- table(NFW$X44); table(NFW$X44)/sum(table(NFW$X44))
X45 <- table(NFW$X45); table(NFW$X45)/sum(table(NFW$X45))
X46 <- table(NFW$X46); table(NFW$X46)/sum(table(NFW$X46))
X47 <- c(0, table(NFW$X47)); table(NFW$X47)/sum(table(NFW$X47))
X48 <- table(NFW$X48); table(NFW$X48)/sum(table(NFW$X48))
X49 <- table(NFW$X49); table(NFW$X49)/sum(table(NFW$X49))
X50 <- table(NFW$X50); table(NFW$X50)/sum(table(NFW$X50))

# Bar plots

barplot(X30, ylab="Frequency", ylim=c(0,60))
barplot(X31, ylab="Frequency", ylim=c(0,60))
barplot(X32, ylab="Frequency", ylim=c(0,60))
barplot(X33, ylab="Frequency", ylim=c(0,60))
barplot(X34, ylab="Frequency", ylim=c(0,60))
barplot(X35, ylab="Frequency", ylim=c(0,60))
barplot(X36, ylab="Frequency", ylim=c(0,60))
barplot(X37, ylab="Frequency", ylim=c(0,60))
barplot(X38, ylab="Frequency", ylim=c(0,60))
barplot(X39, ylab="Frequency", ylim=c(0,60))
barplot(X40, ylab="Frequency", ylim=c(0,60))
barplot(X41, ylab="Frequency", ylim=c(0,60))
barplot(X42, ylab="Frequency", ylim=c(0,60))
barplot(X43, ylab="Frequency", ylim=c(0,60))
barplot(X44, ylab="Frequency", ylim=c(0,60))
barplot(X45, ylab="Frequency", ylim=c(0,60))
barplot(X46, ylab="Frequency", ylim=c(0,60))
barplot(X47, ylab="Frequency", ylim=c(0,60))
barplot(X48, ylab="Frequency", ylim=c(0,60))
barplot(X49, ylab="Frequency", ylim=c(0,60))
barplot(X50, ylab="Frequency", ylim=c(0,60))

# For n, mean, median, sd, and p-values 

describe(NFW[,17:37])

NFW[,17:37]<-NFW[,17:37]/5
describe(NFW[,17:37])

#-----------------------------------------------------------------------------------
# Analysis Question 7
#-----------------------------------------------------------------------------------

# Frequencies of each response (including 0 response)

X52 <- table(NFW$X52); table(NFW$X52)/sum(table(NFW$X52)); X52 <- c("0"=15, "1"=1, 0, "3"=9, "4"=20, "5"=11)
X53 <- table(NFW$X53); table(NFW$X53)/sum(table(NFW$X53))
X54 <- table(NFW$X54); table(NFW$X54)/sum(table(NFW$X54))
X55 <- table(NFW$X55); table(NFW$X55)/sum(table(NFW$X55)); X55 <- c("0"=22, "1"=2, 0, "3"=5, "4"=15, "5"=11)

# Recoding 0=NA

NFW[,38:41][NFW[,38:41]==0] <- NA  

X52 <- table(NFW$X52); table(NFW$X52)/sum(table(NFW$X52)); X52 <- c("1"=1, 0, "3"=9, "4"=20, "5"=11)
X53 <- table(NFW$X53); table(NFW$X53)/sum(table(NFW$X53))
X54 <- table(NFW$X54); table(NFW$X54)/sum(table(NFW$X54))
X55 <- table(NFW$X55); table(NFW$X55)/sum(table(NFW$X55)); X55 <- c("1"=2, 0, "3"=5, "4"=15, "5"=11)

# Bar plots

barplot(X52, ylab="Frequency", ylim=c(0,60))
barplot(X53, ylab="Frequency", ylim=c(0,60))
barplot(X54, ylab="Frequency", ylim=c(0,60))
barplot(X55, ylab="Frequency", ylim=c(0,60))

# For n, mean, median, sd, and p-values 

describe(NFW[,38:41])

NFW[,38:41]<-NFW[,38:41]/5
describe(NFW[,38:41])

#-----------------------------------------------------------------------------------
# Analysis Questions 11-12
#-----------------------------------------------------------------------------------

X60 <- table(NFW$X60); table(NFW$X60)/sum(table(NFW$X60))
X61 <- table(NFW$X61); table(NFW$X61)/sum(table(NFW$X61))

barplot(X60, ylab="Frequency", ylim=c(0,60))
barplot(X61, ylab="Frequency", ylim=c(0,60))

describe(NFW[,42:43])

NFW[,42:43]<-NFW[,42:43]/4
describe(NFW[,42:43])

#-----------------------------------------------------------------------------------
# Analysis Questions 11-12
#-----------------------------------------------------------------------------------

X62 <- table(NFW$X62); table(NFW$X62)/sum(table(NFW$X62)); X62 <- c("1"=2, 0, "3"=2, "4"=16, "5"=36)
X63 <- table(NFW$X63); table(NFW$X63)/sum(table(NFW$X63))
X64 <- c(0, table(NFW$X64)); table(NFW$X64)/sum(table(NFW$X64))
X65 <- table(NFW$X65); table(NFW$X65)/sum(table(NFW$X65))

barplot(X62, ylab="Frequency", ylim=c(0,60))
barplot(X63, ylab="Frequency", ylim=c(0,60))
barplot(X64, ylab="Frequency", ylim=c(0,60))
barplot(X65, ylab="Frequency", ylim=c(0,60))

describe(NFW[,44:47])

NFW[,44:47]<-NFW[,44:47]/5
describe(NFW[,44:47])















X29


barplot(X14, ylab="Frequency", ylim=c(0,60))












sjp.likert(d[, 2:16], 
           catcount = 4, 
           legendLabels=c("I have been in a class (as a student) which used this technique",
                          "I have used this technique as a teacher (currently or in the past)", 
                          "I have heard of this technique",
                          "I have never heard of it / Not Sure",
                          " ",
                          " "),
           sort.frq="neg.desc",
           value.labels="sum.outside",
           geom.colors="Greys",
           showPercentageSign=TRUE,
           reverse.colors=TRUE,
           expand.grid=TRUE,
           includeN=TRUE,
           axisLabels.y=c(
             "11.a. Think-Pair-Share",
             "11.b. Peer Instruction",
             "11.c. Just-in-time Teaching",
             "11.d. SCALE-UP Classroom", 
             "12.a. Interactive Lecture Demonstrations",
             "12.b. Cooperative group problem solving",
             "12.c. PhET Interactive Simulations",
             "12.d. Open Source Physics",
             "12.e. Ranking tasks",
             "12.f. Real Time Physics",
             "12.g. Tutorials in Introductory Physics",
             "13.a. Concept Tests/Inventories"
           ),
           gridRange=1.1,
           title="Frequency Distribution of Responses"
) 
