setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 EFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 01. Demographics
#---------------------------------------------------------------------------

# Gender 
# ~~~~~~

# Frequency table of unique responses
table(EFW.pre$X79)/sum(table(EFW.pre$X79))
table(EFW.post$X64)/sum(table(EFW.post$X64))
table(EFW.pp$X79)/sum(table(EFW.pp$X79))
table(EFW.pp$X64.y)/sum(table(EFW.pp$X64.y))
# View(EFW.pp[,c(1:10,75,141)])

# Bar plot
bar <- barplot(table(EFW.pre$X79), 
               ylab="Frequency", ylim=c(0,35),
               names.arg=c("Male", "Female", "Prefer not \nto answer"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(EFW.pre$X79), 
     label = table(EFW.pre$X79), 
     pos = 3, cex = 0.8, col = "black")

bar <- barplot(table(EFW.post$X64), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Male", "Female", "Prefer not \nto answer"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(EFW.post$X64), 
     label = table(EFW.post$X64), 
     pos = 3, cex = 0.8, col = "black")


# Institution Type 
# ~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(EFW.pre$X15)/sum(table(EFW.pre$X15))
table(EFW.post$X14)/sum(table(EFW.post$X14))
table(EFW.pp$X15.x)/sum(table(EFW.pp$X15.x))
table(EFW.pp$X14.y)/sum(table(EFW.pp$X14.y))
# View(EFW.pp[,c(1:11, 91)])

# Bar plot
bar <- barplot(table(EFW.pre$X15), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Primarily \nUndergraduate", 
                           "Master's Degree \nin Physics", 
                           "PhD in \n Physics"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(EFW.pre$X15), 
     label = table(EFW.pre$X15), 
     pos = 3, cex = 0.8, col = "black")
bar <- barplot(table(EFW.post$X14), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Primarily \nUndergraduate", 
                           "Master's Degree \nin Physics", 
                           "PhD in \n Physics"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(EFW.post$X14), 
     label = table(EFW.post$X14), 
     pos = 3, cex = 0.8, col = "black")


# Ethnicity 
# ~~~~~~~~~

# Frequency table of unique responses
EFW.pre$ethnicity <- rowSums(EFW.pre[,76:79], na.rm=TRUE)
table(EFW.pre$ethnicity)/40
EFW.post$ethnicity <- rowSums(EFW.post[,61:64], na.rm=TRUE)
table(EFW.post$ethnicity)/33

EFW.pp$ethnicity.pre <- rowSums(EFW.pp[,76:79], na.rm=TRUE)
EFW.pp$ethnicity.post <- rowSums(EFW.pp[, 142:145], na.rm=TRUE)
table(EFW.pp$ethnicity.pre)/31
table(EFW.pp$ethnicity.post)/31

# Bar plot
bar.pre <- barplot(c(5,2,27,6), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Asian or \n Pacific Islander", 
                           "Hispanic or Latino & \nWhite/Caucasian", 
                           "White/ \nCaucasian",
                           "Prefer not \nto answer"),
               cex.names=0.7, cex.axis=0.8)
text(x = bar.pre, 
     y = c(5,2,27,6), 
     label = c(5,2,27,6), 
     pos = 3, cex = 0.8, col = "black")
bar.post <- barplot(c(4,2,22,5), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Asian or \n Pacific Islander", 
                           "Hispanic or Latino & \nWhite/Caucasian", 
                           "White/ \nCaucasian",
                           "Prefer not \nto answer"),
               cex.names=0.7, cex.axis=0.8)
text(x = bar.post, 
     y = c(4,2,22,5), 
     label = c(4,2,22,5), 
     pos = 3, cex = 0.8, col = "black")


# Degree 
# ~~~~~~

# Frequency table of unique responses
table(EFW.pre$X86)/sum(table(EFW.pre$X86))
table(EFW.pre$X87)/sum(table(EFW.pre$X87))
table(EFW.post$X71)/sum(table(EFW.post$X71))
table(EFW.post$X72)/sum(table(EFW.post$X72))
table(EFW.pp$X86)/sum(table(EFW.pp$X86))
table(EFW.pp$X87)/sum(table(EFW.pp$X87))
table(EFW.pp$X71.y)/sum(table(EFW.pp$X71.y))
table(EFW.pp$X72.y)/sum(table(EFW.pp$X72.y))
View(EFW.pp[,c(1:10,80:81,146:147)])

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

bar.pre <- barplot(matrix(c(1,1,6,4,7,4,23,31,1,0),nr=2), 
               beside=T, 
               col=c("white","gray50"), 
               names.arg=c("Antartica", "Asia", "Europe", 
                           "North \nAmerica", "South \nAmerica"),
               ylab="Frequency", ylim=c(0,35),
               cex.names=0.8, cex.axis=0.8)
text(x = bar.pre, 
     y = matrix(c(1,1,6,4,7,4,23,31,1,0),nr=2), 
     label = matrix(c(1,1,6,4,7,4,23,31,1,0),nr=2), 
     pos = 3, cex = 0.8, col = "black")
legend(1, 40, 
       legend=c("Undergraduate degree \n(Bachelor's or equivalent)","Doctoral degree"), 
       col=c("gray10","gray50"),
       fill=c("white","gray50"),
       border="black",
       bty="n", y.intersp=0.5, cex = 0.75)

bar.post <- barplot(matrix(c(5,2,5,3,22,28,1,0),nr=2), 
               beside=T, 
               col=c("white","gray50"), 
               names.arg=c("Asia", "Europe", 
                           "North \nAmerica", "South \nAmerica"),
               ylab="Frequency", ylim=c(0,35),
               cex.names=0.8, cex.axis=0.8)
text(x = bar.post, 
     y = matrix(c(5,2,5,3,22,28,1,0),nr=2), 
     label = matrix(c(5,2,5,3,22,28,1,0),nr=2), 
     pos = 3, cex = 0.8, col = "black")
legend(1, 40, 
       legend=c("Undergraduate degree \n(Bachelor's or equivalent)","Doctoral degree"), 
       col=c("gray10","gray50"),
       fill=c("white","gray50"),
       border="black",
       bty="n", y.intersp=0.5, cex = 0.75)


