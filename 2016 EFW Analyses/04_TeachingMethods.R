setwd("~/Dropbox/EFW Analyses/Analyses")
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

EFW[,c(26:29, 32:38, 41:48)] <- 6 - EFW[,c(26:29, 32:38, 41:48)]

# Recode numbers to value labels and convert to factors
levels = c("5 = I have never heard of it/Not sure", 
           "4 = I've heard of the name but do not know much else about it", 
           "3 = I am familar with it but have never used it", 
           "2 = I have used all or part of it in the past", 
           "1 = I currently use all or part of it")
EFW[,c(26:29, 32:38, 41:48)] <- recode(EFW[,c(26:29, 32:38, 41:48)],
                                       from=c(5, 4, 3, 2, 1), to=levels)
EFW[,c(26:29, 32:38, 41:48)] <- lapply(EFW[,c(26:29, 32:38, 41:48)], factor)
sapply(EFW[,c(26:29, 32:38, 41:48)], class)

# Run a likert analysis for question 14
tchapp <- as.data.frame(EFW[,26:29])
tchapp <- rename(tchapp, c(X30="Peer Instruction or Think Pair Share",
                           X31="Just-in-time Teaching",
                           X32="SCALE-UP or Studio Classroom",
                           X33="Lecture Tutorials for Introductory Astronomy"))
tchapp <- likert(tchapp)

# Run a likert analysis for question 15
tchcurr <- as.data.frame(EFW[,32:38])
tchcurr <- rename(tchcurr, 
                  c(X36="Interactive Lecture Demonstrations", 
                    X37="Cooperative group problem solving", 
                    X38="PhET Interactive Simulations", 
                    X39="Open Source Physics", 
                    X40="Ranking tasks", 
                    X41="Real Time Physics", 
                    X42="Tutorials in Introductory Physics"))
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
tchcurr <- likert(tchcurr)

# Run a likert analysis for question 16
tchass <- as.data.frame(EFW[,41:48])
tchass <- rename(tchass, 
                 c(X45="Force Concept Inventory (FCI)",
                   X46="Mechanics Baseline Test (MBT)",
                   X47="Conceptual Survey in Electricity & Magnetism (CSEM)",
                   X48="Colorado Learning Attitudes about Science Survey (CLASS)", 
                   X49="Brief Electricity and Magnetism Assessment (BEMA)", 
                   X50="Maryland Physics Expectations Survey (MPEX)",
                   X51="Light and Spectroscopy Concept Inventory (LSCI)", 
                   X52="Classroom observation protocols (COPUS or TDOP)"))
for(i in seq_along(tchass)) {
  tchass[,i] <- factor(tchass[,i], levels=c(
    "1 = I currently use all or part of it",
    "2 = I have used all or part of it in the past",
    "3 = I am familar with it but have never used it", 
    "4 = I've heard of the name but do not know much else about it",
    "5 = I have never heard of it/Not sure"))
}
tchass <- likert(tchass)

# Create plots
p1 <- plot(tchapp, centered=FALSE, ordered=FALSE,
           group.order=c(
             X30="Peer Instruction or Think Pair Share",
             X31="Just-in-time Teaching",
             X32="SCALE-UP or Studio Classroom",
             X33="Lecture Tutorials for Introductory Astronomy"
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
             X38="PhET Interactive Simulations",
             X37="Cooperative group problem solving",
             X36="Interactive Lecture Demonstrations", 
             X40="Ranking tasks", 
             X39="Open Source Physics", 
             X42="Tutorials in Introductory Physics",
             X41="Real Time Physics"
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
p3 <- plot(tchass, centered=FALSE, ordered=FALSE,
           group.order=c(
             X45="Force Concept Inventory (FCI)",
             X47="Conceptual Survey in Electricity & Magnetism (CSEM)",
             X46="Mechanics Baseline Test (MBT)",
             X48="Colorado Learning Attitudes about Science Survey (CLASS)", 
             X51="Light and Spectroscopy Concept Inventory (LSCI)", 
             X49="Brief Electricity and Magnetism Assessment (BEMA)",
             X52="Classroom observation protocols (COPUS or TDOP)",
             X50="Maryland Physics Expectations Survey (MPEX)"
           ),
           plot.percents=TRUE, wrap=30,
           plot.percent.low=FALSE, plot.percent.high=FALSE, 
           colors=c('gray35','gray50','gray65','gray80','gray95')) +
  theme_gray() + 
  labs(title="Assessments") +
  theme(legend.position="bottom", legend.title=element_blank(),
        text = element_text(colour = "black", size=14),
        axis.text.y = element_text(colour = "black", size=12),
        strip.text=element_text(colour="black", size=14)) + 
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
