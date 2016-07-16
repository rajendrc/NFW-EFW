setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 04. Familarity with Teaching Methods
#---------------------------------------------------------------------------

library(likert)
library(plyr)
library(ggplot2)
library(gridExtra)

# Graphical Summary 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NFW.pre[,32:43] <- 6 - NFW.pre[,32:43]

# Recode numbers to value labels and convert to factors
levels = c("5 = I have never heard of it/Not sure", 
           "4 = I've heard of the name but do not know much else about it", 
           "3 = I am familar with it but have never used it", 
           "2 = I have used all or part of it in the past", 
           "1 = I currently use all or part of it")
NFW.pre[,32:43] <- recode(NFW.pre[,32:43], from=c(5, 4, 3, 2, 1), to=levels)
NFW.pre[,32:43] <- lapply(NFW.pre[,32:43], factor)
sapply(NFW.pre[,32:43], class)

# Run a likert analysis for question 12
tchapp <- as.data.frame(NFW.pre[,32:34])
tchapp <- rename(tchapp, c(X37="Think-Pair-Share or Peer Instruction",
                           X38="Just-in-time Teaching",
                           X39="SCALE-UP or Studio Classroom"))
tchapp <- likert(tchapp)

# Run a likert analysis for question 13
tchcurr <- as.data.frame(NFW.pre[,35:42])
tchcurr <- rename(tchcurr, 
                  c(X41="Interactive Lecture Demonstrations", 
                    X42="Cooperative group problem solving", 
                    X43="PhET Interactive Simulations", 
                    X44="Open Source Physics", 
                    X45="Ranking tasks", 
                    X46="Real Time Physics", 
                    X47="Tutorials in Introductory Physics",
                    X48="Lecture Tutorials for Introductory Astronomy"))
tchcurr <- likert(tchcurr)

# Run a likert analysis for question 14
tchass <- as.data.frame(NFW.pre[,43])
colnames(tchass) <- "Concept Tests/Inventories (e.g., Force Concept Inventory)."
tchass <- likert(tchass)

# Create plots
p1 <- plot(tchapp, centered=FALSE, ordered=FALSE,
           group.order=c(
             X37="Think-Pair-Share or Peer Instruction",
             X38="Just-in-time Teaching",
             X39="SCALE-UP or Studio Classroom"
             ),
           plot.percents=TRUE, wrap=30,
           plot.percent.low=FALSE, plot.percent.high=FALSE,
           colors=c('gray35','gray50','gray65','gray80','gray95')) +
  theme_gray() + 
  labs(title="Published Teaching Approaches") +
  theme(legend.position="bottom", legend.title=element_blank(),
        text = element_text(colour = "black", size=14),
        axis.text.y = element_text(colour = "black", size=12),
        strip.text=element_text(colour="black", size=14)) + 
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
p2 <- plot(tchcurr, centered=FALSE, ordered=FALSE,
           group.order=c(
             X42="Cooperative group problem solving", 
             X43="PhET Interactive Simulations", 
             X41="Interactive Lecture Demonstrations", 
             X44="Open Source Physics", 
             X45="Ranking tasks", 
             X48="Lecture Tutorials for Introductory Astronomy",
             X46="Real Time Physics", 
             X47="Tutorials in Introductory Physics"
           ),
           plot.percents=TRUE, wrap=30,
           plot.percent.low=FALSE, plot.percent.high=FALSE,
           colors=c('gray35','gray50','gray65','gray80','gray95')) +
  theme_gray() + 
  labs(title="Published Curriculum/Products") +
  theme(legend.position="bottom", legend.title=element_blank(),
        text = element_text(colour = "black", size=14),
        axis.text.y = element_text(colour = "black", size=12),
        strip.text=element_text(colour="black", size=14)) + 
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
p3 <- plot(tchass, centered=FALSE, ordered=FALSE, plot.percents=TRUE, wrap=30,
           plot.percent.low=FALSE, plot.percent.high=FALSE, 
           colors=c('gray35','gray50','gray65','gray80','gray95')) +
  theme_gray() + 
  labs(title="Assessments") +
  theme(legend.position="bottom", legend.title=element_blank(),
        text = element_text(colour = "black", size=14),
        axis.text.y = element_text(colour = "black", size=12),
        strip.text=element_text(colour="black", size=14)) + 
  guides(fill=guide_legend(nrow=3,byrow=TRUE))



###
tchcurr[,6] <- factor(tchcurr[,6], levels=levels)

for(i in seq_along(tchcurr)) {
  tchcurr[,i] <- factor(tchcurr[,i], levels=c(
    "1 = I currently use all or part of it",
    "2 = I have used all or part of it in the past",
    "3 = I am familar with it but have never used it", 
    "4 = I've heard of the name but do not know much else about it",
    "5 = I have never heard of it/Not sure")
  )
}
