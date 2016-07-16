setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

library(sjPlot)
library(psych)

#---------------------------------------------------------------------------
# 07. Response to workshop as a whole: Closed chioce
#---------------------------------------------------------------------------

# Reverse coding so Strongly agree=4
Jun16.NFW.post[,74:88] <- 5- Jun16.NFW.post[,17:31]

# Frequencies of each response
X21 <- c("1"==0, table(Jun16.NFW.post$X21.1)); prop.table(X21)
X22 <- c("1"==0, table(Jun16.NFW.post$X22.1)); prop.table(X22)
X23 <- table(Jun16.NFW.post$X23.1); prop.table(X23)
X24 <- table(Jun16.NFW.post$X24.1); prop.table(X24)
X25 <- table(Jun16.NFW.post$X25.1); prop.table(X25)
X26 <- c("1"==0, table(Jun16.NFW.post$X26.1)); prop.table(X26)
X27 <- c("1"==0, table(Jun16.NFW.post$X27.1)); prop.table(X27)
X28 <- table(Jun16.NFW.post$X28.1); prop.table(X28)
X29 <- c("1"==0, table(Jun16.NFW.post$X29.1)); prop.table(X29)
X30 <- table(Jun16.NFW.post$X30.1); prop.table(X30)
X31 <- c("1"==0, table(Jun16.NFW.post$X31.1)); prop.table(X31) 
X32 <- c("1"==0, table(Jun16.NFW.post$X32.1)); prop.table(X32) 
X33 <- c("1"==0, table(Jun16.NFW.post$X33.1)); prop.table(X33)
X34 <- c("1"==0, table(Jun16.NFW.post$X34.1)); prop.table(X34)
X35 <- c("1"==0, table(Jun16.NFW.post$X35.1)); prop.table(X35)

# Barplots
barplot(X21, ylab="Frequency", ylim=c(0,30))
barplot(X22, ylab="Frequency", ylim=c(0,30))
barplot(X23, ylab="Frequency", ylim=c(0,30))
barplot(X24, ylab="Frequency", ylim=c(0,30))
barplot(X25, ylab="Frequency", ylim=c(0,30))
barplot(X26, ylab="Frequency", ylim=c(0,30))
barplot(X27, ylab="Frequency", ylim=c(0,30))
barplot(X28, ylab="Frequency", ylim=c(0,30))
barplot(X29, ylab="Frequency", ylim=c(0,30))
barplot(X30, ylab="Frequency", ylim=c(0,30))
barplot(X31, ylab="Frequency", ylim=c(0,30))
barplot(X32, ylab="Frequency", ylim=c(0,30))
barplot(X33, ylab="Frequency", ylim=c(0,30))
barplot(X34, ylab="Frequency", ylim=c(0,30))
barplot(X35, ylab="Frequency", ylim=c(0,30))

# For n, mean, median, sd, and p-values 
describe(Jun16.NFW.post[,74:88])
Jun16.NFW.post[,74:88] <- Jun16.NFW.post[,74:88]/4
describe(Jun16.NFW.post[,74:88])

# Likert bar plots:
sjp.setTheme(theme="theme_bw", title.align="center",
             title.size=1.8,
             axis.title.size=1.5,
             geom.label.size=5,
             legend.size=1,
             axis.textcolor.x="Black",
             axis.textcolor.y="Black")

sjp.likert(Jun16.NFW.post[,17:31], 
           catcount = 4, 
           legendLabels=c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"),
           value.labels="sum.outside",
           geom.colors="Greys",
           showPercentageSign=TRUE,
           reverse.colors=TRUE,
           expand.grid=TRUE,
           sort.frq = "pos.desc",
           includeN=TRUE,
           axisLabels.y=c(
             "7.a. The overall quality of the workshop exceeded that of other professional development workshops I have attended.",
             "7.b. I gained a broad perspective of teaching techniques through the diversity of topics presented.",
             "7.c. I would have preferred fewer topics with more time per topic.",
             "7.d. I had adequate time to digest and process information presented.", 
             "7.e. The meeting schedule was too exhausting.",
             "7.f. I have a good sense of what many of these teaching techniques look like in the classroom (including relevant classroom examples).",
             "7.g. Meeting sessions gave adequate opportunities for active learning (through discussions, trying out techniques, etc.).",
             "7.h. Meeting sessions relied too much on presentation and lecture.",
             "7.i. I would have liked more opportunity to practice what I learned at the conference.",
             "7.j. I felt that presenters spent too much time explaining or justifying why their technique is effective.",
             "7.k. Presenters indicated that educational techniques were adaptable to a variety of situations.",
             "7.l. I had enough time to interact with my faculty colleagues.",
             "7.m. The workshops built on my existing interests and teaching experience.",
             "7.n. Information provided in advance of the workshop (e.g., emails, articles, schedule) was adequate for my preparation.",
             "7.o. Information and materials provided at the workshops (e.g., USB drive, handouts) were adequate for my future reference."
           ),
           gridRange=1.1,
           title="Question 7 \n Frequency Distribution of Responses"
) 

# Likert plots split into 2
sjp.setTheme(theme="theme_bw", title.align="center",
             title.size=1.8,
             axis.title.size=1.5,
             geom.label.size=5,
             legend.size=1, axis.textsize=1.1,
             axis.textcolor.x="Black",
             axis.textcolor.y="Black")
sjp.likert(Jun16.NFW.post[,c(17:18,22:23,27:29,31)], 
           catcount = 4, 
           legendLabels=c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"),
           value.labels="sum.outside",
           geom.colors="Greys",
           showPercentageSign=TRUE,
           reverse.colors=TRUE,
           expand.grid=TRUE,
           sort.frq = "pos.desc",
           includeN=TRUE,
           axisLabels.y=c(
             "7.a. The overall quality of the workshop exceeded that of other professional development workshops I have attended.",
             "7.b. I gained a broad perspective of teaching techniques through the diversity of topics presented.",
             "7.f. I have a good sense of what many of these teaching techniques look like in the classroom (including relevant classroom examples).",
             "7.g. Meeting sessions gave adequate opportunities for active learning (through discussions, trying out techniques, etc.).",
             "7.k. Presenters indicated that educational techniques were adaptable to a variety of situations.",
             "7.l. I had enough time to interact with my faculty colleagues.",
             "7.m. The workshops built on my existing interests and teaching experience.",
             "7.o. Information and materials provided at the workshops (e.g., USB drive, handouts) were adequate for my future reference."
           ),
           gridRange=1.1,
           title="Question 7 \n Frequency Distribution of Responses - Part I"
) 

sjp.likert(Jun16.NFW.post[,c(19:21, 24:26, 30)], 
           catcount = 4, 
           legendLabels=c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"),
           value.labels="sum.outside",
           geom.colors="Greys",
           showPercentageSign=TRUE,
           reverse.colors=TRUE,
           expand.grid=TRUE,
           sort.frq = "pos.desc",
           includeN=TRUE,
           axisLabels.y=c(
             "7.c. I would have preferred fewer topics with more time per topic.",
             "7.d. I had adequate time to digest and process information presented.", 
             "7.e. The meeting schedule was too exhausting.",
             "7.h. Meeting sessions relied too much on presentation and lecture.",
             "7.i. I would have liked more opportunity to practice what I learned at the conference.",
             "7.j. I felt that presenters spent too much time explaining or justifying why their technique is effective.",
             "7.n. Information provided in advance of the workshop (e.g., emails, articles, schedule) was adequate for my preparation."
           ),
           gridRange=1.1,
           title="Question 7 \n Frequency Distribution of Responses - Part II"
) 



