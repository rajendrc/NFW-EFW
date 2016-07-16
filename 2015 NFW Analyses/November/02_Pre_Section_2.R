#-----------------------------------------------------------------------------------
# Load Clean Data File
#-----------------------------------------------------------------------------------

setwd("~/Documents/Rajendra Chattergoon/CU Boulder/Consulting/Stephanie Chasteen/2015-16 NFW/1516 NFW Survey")
source("Raw_Data.R")

#-----------------------------------------------------------------------------------
# Analysis - Section 2
#-----------------------------------------------------------------------------------

library(psych)
library(ltm)
library(sjPlot)
# library(qgraph) # need to fix this

describe(NFW[,23:32])

d = data.frame(matrix(NA, nrow=10, ncol=2))
d[,1] <- c(
  "a. I want to be a better teacher, \n in general.",
  "b. I want to learn about the evidence \n of active-learning techniques.",
  "c. I want to increase my use of \n active-learning techniques \n in my classroom.",
  "d. I want to use active-learning \n techniques in my classroom \n more effectively in my classroom.",
  "e. I want to improve student \n engagement in my classroom.",
  "f. I want to learn strategies \n for preparing better lectures.",
  "g. My department chair \n encouraged me to attend.",
  "h. Colleagues encouraged \n me to attend.",
  "i. There is a strong expectation \n in my department that new faculty \n attend the workshop.",
  "j. I want to interact with other \n new faculty from other institutions."
)
d[,2] <- c(54, 12, 33, 28, 45, 34, 21, 7, 4, 21) # update this with frequencies from earlier

d <- d[order(d[,2]),]

op <- par(mar=c(5,12,5,2))
barplot(d[,2],  
        names.arg=d[,1], 
        cex.names=0.8,
        cex.main=1,
        main="Q09. My TOP THREE OR FOUR reasons for attending the \n Physics and Astronomy New Faculty Workshop are:",
        xlim=c(0,60),
        xlab="Frequency",
        las=1,
        horiz=TRUE)

X34 <- table(NFW$X34); table(NFW$X34)/sum(table(NFW$X34))
X35 <- table(NFW$X35); table(NFW$X35)/sum(table(NFW$X35))
X36 <- table(NFW$X36); table(NFW$X36)/sum(table(NFW$X36))
X37 <- table(NFW$X37); table(NFW$X37)/sum(table(NFW$X37))
X38 <- table(NFW$X38); table(NFW$X38)/sum(table(NFW$X38))
X39 <- table(NFW$X39); table(NFW$X39)/sum(table(NFW$X39))
X40 <- table(NFW$X40); table(NFW$X40)/sum(table(NFW$X40))
X41 <- table(NFW$X41); table(NFW$X41)/sum(table(NFW$X41))
X42 <- table(NFW$X42); table(NFW$X42)/sum(table(NFW$X42))
X43 <- table(NFW$X43); table(NFW$X43)/sum(table(NFW$X43))

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



NFW$X34m <- NFW$X34/3
NFW$X35m <- NFW$X35/3
NFW$X36m <- NFW$X36/3
NFW$X37m <- NFW$X37/3
NFW$X38m <- NFW$X38/3
NFW$X39m <- NFW$X39/3
NFW$X40m <- NFW$X40/3
NFW$X41m <- NFW$X41/3
NFW$X42m <- NFW$X42/3
NFW$X43m <- NFW$X43/3

describe(NFW[,133:142], na.rm=TRUE)
describe(NFW[ ,34:43])

sjp.likert(NFW[ , 34:43], 
           catcount = 2, 
           cat.neutral=2,
           legendLabels=c("Not Interested", "Very Interested", "Somewhat Interested"),
           value.labels="sum.outside",
           geom.colors="Blues",
           sort.frq="neg.desc",
           cat.neutral.color = "Grey",
           showPercentageSign=TRUE,
           expand.grid=TRUE,
           includeN=TRUE,
           gridRange=1.5, 
           axisLabels.y=c(
             "a. Teaching introductory physics courses",
             "b. Teaching introductory astronomy courses",
             "c. Teaching upper-level physics courses",
             "d. Teaching upper-level astronomy courses",
             "e. Teaching graduate courses",
             "f. Teaching laboratory courses",
             "g. Increasing diversity in physics and astronomy",
             "h. Retention of undergraduate majors",
             "i. Assessing student learning",
             "j. Overcoming challenges or obstacles in changing my teaching approach"
           ),
           title="Q10. Which of the following topics are you most interested in learning about?"
) 

cor(NFW[ ,34:43], use="complete.obs")

# Nathan's Code for Network Analysis:
# jpeg(filename="out/co_interest.jpeg",  width=800, height=800)
# par(omi=c(0,0,0,0))
# qgraph(co, layout = "spring",  edge.labels=TRUE, edge.label.cex=1.4,
#       label.cex=2)
