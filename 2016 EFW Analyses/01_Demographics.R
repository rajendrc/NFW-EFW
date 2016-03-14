setwd("~/Dropbox/EFW Analyses/Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 01. Demographics
#---------------------------------------------------------------------------

# Gender 
# ~~~~~~

# Frequency table of unique responses
table(EFW$X79)/sum(table(EFW$X79))

# Bar plot
bar <- barplot(table(EFW$X79), 
               ylab="Frequency", ylim=c(0,35),
               names.arg=c("Male", "Female", "Prefer not \nto answer"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(EFW$X79), 
     label = table(EFW$X79), 
     pos = 3, cex = 0.8, col = "black")

# Institution Type 
# ~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(EFW$X15)/sum(table(EFW$X15))

# Bar plot
bar <- barplot(table(EFW$X15), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Primarily \nUndergraduate", 
                           "Master's Degree \nin Physics", 
                           "PhD in \n Physics"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(EFW$X15), 
     label = table(EFW$X15), 
     pos = 3, cex = 0.8, col = "black")

# Ethnicity 
# ~~~~~~~~~

# Frequency table of unique responses
EFW$ethnicity <- rowSums(EFW[,76:79], na.rm=TRUE)
table(EFW$ethnicity)/40

# Bar plot
bar <- barplot(c(5,2,27,6), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Asian or \n Pacific Islander", 
                           "Hispanic or Latino & \nWhite/Caucasian", 
                           "White/ \nCaucasian",
                           "Prefer not \nto answer"),
               cex.names=0.7, cex.axis=0.8)
text(x = bar, 
     y = c(5,2,27,6), 
     label = c(5,2,27,6), 
     pos = 3, cex = 0.8, col = "black")


# Degree 
# ~~~~~~

# Frequency table of unique responses
table(EFW$X86)/sum(table(EFW$X86))
table(EFW$X87)/sum(table(EFW$X87))

# Barplot of degree information
bar <- barplot(matrix(c(1,1,6,4,7,4,23,31,1,0),nr=2), 
               beside=T, 
               col=c("white","gray50"), 
               names.arg=c("Antartica", "Asia", "Europe", 
                           "North \nAmerica", "South \nAmerica"),
               ylab="Frequency", ylim=c(0,35),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = matrix(c(1,1,6,4,7,4,23,31,1,0),nr=2), 
     label = matrix(c(1,1,6,4,7,4,23,31,1,0),nr=2), 
     pos = 3, cex = 0.8, col = "black")
legend(1, 40, 
       legend=c("Undergraduate degree \n(Bachelor's or equivalent)","Doctoral degree"), 
       col=c("gray10","gray50"),
       fill=c("white","gray50"),
       border="black",
       bty="n", y.intersp=0.5, cex = 0.75)
