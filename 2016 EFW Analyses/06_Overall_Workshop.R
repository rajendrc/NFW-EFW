setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 EFW Analyses")
source("00_CleanData.R")

library(sjPlot)
library(psych)

#---------------------------------------------------------------------------
# 06. Response to workshop as a whole: Closed chioce
#---------------------------------------------------------------------------

# Reverse coding so Strongly agree=4
EFW.post[,67:81] <- 5- EFW.post[,11:25]

# Frequencies of each response
X15 <- c("1"==0, table(EFW.post$X15.1)); prop.table(X15) 
X16 <- c("1"==0, table(EFW.post$X16.1)); prop.table(X16)
X17 <- table(EFW.post$X17.1); prop.table(X17)
X18 <- c("1"==0, table(EFW.post$X18.1)); prop.table(X18)
X19 <- table(EFW.post$X19.1); prop.table(X19)
X20 <- c("1"==0, table(EFW.post$X20.1)); prop.table(X20)
X21 <- table(EFW.post$X21.1); prop.table(X21)
X22 <- table(EFW.post$X22.1); prop.table(X22)
X23 <- table(EFW.post$X23.1); prop.table(X23)
X24 <- table(EFW.post$X24.1); prop.table(X24)
X25 <- c("1"==0, table(EFW.post$X25.1)); prop.table(X25)
X26 <- c("1"==0, table(EFW.post$X26.1)); prop.table(X26)
X27 <- c("1"==0, table(EFW.post$X27.1)); prop.table(X27)
X28 <- table(EFW.post$X28.1); prop.table(X28)
X29 <- table(EFW.post$X29.1); prop.table(X29)

# Bar plots
barplot(X15, ylab="Frequency", ylim=c(0,30))
barplot(X16, ylab="Frequency", ylim=c(0,30))
barplot(X17, ylab="Frequency", ylim=c(0,30))
barplot(X18, ylab="Frequency", ylim=c(0,30))
barplot(X19, ylab="Frequency", ylim=c(0,30))
barplot(X20, ylab="Frequency", ylim=c(0,30))
barplot(X21, ylab="Frequency", ylim=c(0,30))
barplot(X22, ylab="Frequency", ylim=c(0,30))
barplot(X23, ylab="Frequency", ylim=c(0,30))
barplot(X24, ylab="Frequency", ylim=c(0,30))
barplot(X25, ylab="Frequency", ylim=c(0,30))
barplot(X26, ylab="Frequency", ylim=c(0,30))
barplot(X27, ylab="Frequency", ylim=c(0,30))
barplot(X28, ylab="Frequency", ylim=c(0,30))
barplot(X29, ylab="Frequency", ylim=c(0,30))

# For n, mean, median, sd, and p-values 
describe(EFW.post[,67:81])
EFW.post[,67:81] <- EFW.post[,67:81]/4
describe(EFW.post[,67:81])

# Likert bar plots:
sjp.setTheme(theme="theme_bw", title.align="center",
             title.size=1.8,
             axis.title.size=1.5,
             geom.label.size=5,
             legend.size=1,
             axis.textcolor.x="Black",
             axis.textcolor.y="Black")

sjp.likert(EFW.post[,11:25], 
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
             "6.a. The overall quality of the workshop exceeded that of other professional development workshops I have attended.",
             "6.b. I gained a broad perspective of teaching techniques through the diversity of topics presented.",
             "6.c. I would have preferred fewer topics with more time per topic.",
             "6.d. I had adequate time to digest and process information presented.", 
             "6.e. The meeting schedule was too exhausting.",
             "6.f. I have a good sense of what many of these teaching techniques look like in the classroom (including relevant classroom examples).",
             "6.g. Meeting sessions gave adequate opportunities for active learning (through discussions, trying out techniques, etc.).",
             "6.h. Meeting sessions relied too much on presentation and lecture.",
             "6.i. I would have liked more opportunity to practice what I learned at the conference.",
             "6.j. I felt that presenters spent too much time explaining or justifying why their technique is effective.",
             "6.k. Presenters indicated that educational techniques were adaptable to a variety of situations.",
             "6.l. I had enough time to interact with my faculty colleagues.",
             "6.m. The workshops built on my existing interests and teaching experience.",
             "6.n. Information provided in advance of the workshop (e.g., emails, articles, schedule) was adequate for my preparation.",
             "6.o. Information and materials provided at the workshops (e.g., handouts) were adequate for my future reference."
           ),
           gridRange=1.1,
           title="Question 6 \n Frequency Distribution of Responses"
) 

# Likert plots split into 2
sjp.setTheme(theme="theme_bw", title.align="center",
             title.size=1.8,
             axis.title.size=1.5,
             geom.label.size=5,
             legend.size=1, axis.textsize=1.1,
             axis.textcolor.x="Black",
             axis.textcolor.y="Black")

sjp.likert(EFW.post[,c(11:12,14,16,21:23)], 
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
             "6.a. The overall quality of the workshop exceeded that of other professional development workshops I have attended.",
             "6.b. I gained a broad perspective of teaching techniques through the diversity of topics presented.",
             "6.d. I had adequate time to digest and process information presented.", 
             "6.f. I have a good sense of what many of these teaching techniques look like in the classroom (including relevant classroom examples).",
             "6.k. Presenters indicated that educational techniques were adaptable to a variety of situations.",
             "6.l. I had enough time to interact with my faculty colleagues.",
             "6.m. The workshops built on my existing interests and teaching experience."
           ),
           gridRange=1.1,
           title="Question 6 \n Frequency Distribution of Responses - Part I"
) 

sjp.likert(EFW.post[,c(13,15,17:20,24:25)], 
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
             "6.c. I would have preferred fewer topics with more time per topic.",
             "6.e. The meeting schedule was too exhausting.",
             "6.g. Meeting sessions gave adequate opportunities for active learning (through discussions, trying out techniques, etc.).",
             "6.h. Meeting sessions relied too much on presentation and lecture.",
             "6.i. I would have liked more opportunity to practice what I learned at the conference.",
             "6.j. I felt that presenters spent too much time explaining or justifying why their technique is effective.",
             "6.n. Information provided in advance of the workshop (e.g., emails, articles, schedule) was adequate for my preparation.",
             "6.o. Information and materials provided at the workshops (e.g., handouts) were adequate for my future reference."
           ),
           gridRange=1.1,
           title="Question 6 \n Frequency Distribution of Responses - Part II"
) 
