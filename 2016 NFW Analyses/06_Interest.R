setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 06. Interest in attending workshop
#---------------------------------------------------------------------------

library(likert)
library(plyr)
library(ggplot2)
library(gridExtra)

# Graphical Summary 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NFW.pre[,21:30] <- 4 - NFW.pre[,21:30]

# Recode numbers to value labels and convert to factors
levels = c("3 = not interested", 
           "2 = somewhat interested", 
           "1 = very interested")
NFW.pre[,21:30] <- recode(NFW.pre[,21:30], from=c(3, 2, 1), to=levels)
NFW.pre[,21:30] <- lapply(NFW.pre[,21:30], factor)
sapply(NFW.pre[,21:30], class)

# Run a likert analysis for question 11
topics <- as.data.frame(NFW.pre[,21:30])
topics <- rename(topics, c(X26="Teaching introductory physics courses",
                           X27="Teaching introductory astronomy courses",
                           X28="Teaching upper-level physics courses",
                           X29="Teaching upper-level astronomy courses",
                           X30="Teaching graduate courses",
                           X31="Teaching laboratory courses",
                           X32="Increasing diversity in physics and astronomy",
                           X33="Retention of undergraduate majors",
                           X34="Assessing student learning",
                           X35="Overcoming challenges or obstacles in changing my teaching approach"))
for(i in seq_along(topics)) {
  topics[,i] <- factor(topics[,i], levels=c(
    "1 = very interested",
    "2 = somewhat interested",
    "3 = not interested")
  )
}
topics <- likert(topics)

# Create plots
p1 <- plot(topics, centered=FALSE, ordered=FALSE,
           group.order=c(
             X26="Teaching introductory physics courses",
             X28="Teaching upper-level physics courses",
             X35="Overcoming challenges or obstacles in changing my teaching approach",
             X34="Assessing student learning",
             X32="Increasing diversity in physics and astronomy",
             X33="Retention of undergraduate majors",
             X30="Teaching graduate courses",
             X31="Teaching laboratory courses",
             X27="Teaching introductory astronomy courses",
             X29="Teaching upper-level astronomy courses"),
           plot.percents=TRUE, wrap=30,
           plot.percent.low=FALSE, plot.percent.high=FALSE,
           colors=c('gray30','gray60','gray90')) +
  theme_gray() + 
  labs(title="Which of the following topics are you MOST interested in learning about?") +
  theme(legend.position="bottom", legend.title=element_blank(),
        text = element_text(colour = "black", size=14),
        axis.text.y = element_text(colour = "black", size=12),
        strip.text=element_text(colour="black", size=14)) + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE))




