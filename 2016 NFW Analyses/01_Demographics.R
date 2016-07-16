setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 01. Demographics
#---------------------------------------------------------------------------

# Gender 
# ~~~~~~

# Frequency table of unique responses
table(Jun16.NFW.pre$X78)/sum(table(Jun16.NFW.pre$X78))

d <- merge(Jun16.NFW.pre[,c(9,70)], Jun16.NFW.post[,c(9,67)], by="Email", all.y=TRUE)
d$Gender <- ifelse(is.na(d$X71), d$X78, d$X71)
table(d$Gender)/sum(table(d$Gender))

# Bar plots
bar <- barplot(table(Jun16.NFW.pre$X78), 
               ylab="Frequency", ylim=c(0,40),
               names.arg=c("Male", "Female", "Prefer not \nto answer"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(Jun16.NFW.pre$X78), 
     label = table(Jun16.NFW.pre$X78), 
     pos = 3, cex = 0.8, col = "black")

bar <- barplot(c(table(d$Gender),0), 
               ylab="Frequency", ylim=c(0,40),
               names.arg=c("Male", "Female", "Prefer not \nto answer"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = c(table(d$Gender),0), 
     label = c(table(d$Gender),0), 
     pos = 3, cex = 0.8, col = "black")


# Institution Type 
# ~~~~~~~~~~~~~~~~

# Frequency table of unique responses
table(Jun16.NFW.pre$X14)/sum(table(Jun16.NFW.pre$X14))

d <- merge(Jun16.NFW.pre[,9:10], Jun16.NFW.post[,c(9,64)], by="Email", all.y=TRUE)
d$Inst <- ifelse(is.na(d$X68), d$X14, d$X68)
table(d$Inst)/sum(table(d$Inst))

# Bar plot
bar <- barplot(table(Jun16.NFW.pre$X14), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Primarily \nUndergraduate", 
                           "Master's Degree \ngranting", 
                           "PhD \ngranting"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(Jun16.NFW.pre$X14), 
     label = table(Jun16.NFW.pre$X14), 
     pos = 3, cex = 0.8, col = "black")

bar <- barplot(table(d$Inst), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("Primarily \nUndergraduate", 
                           "Master's Degree \ngranting", 
                           "PhD \ngranting"),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = table(d$Inst), 
     label = table(d$Inst), 
     pos = 3, cex = 0.8, col = "black")


# Ethnicity 
# ~~~~~~~~~

# Frequency table of unique responses
Jun16.NFW.pre$ethnicity <- rowSums(Jun16.NFW.pre[,71:76], na.rm=TRUE)
table(Jun16.NFW.pre$ethnicity)/sum(table(Jun16.NFW.pre$ethnicity))

d <- merge(Jun16.NFW.pre[,c(9,71:76)], Jun16.NFW.post[,c(9,68:69)], by="Email", all.y=TRUE)
d$White <- ifelse(is.na(d$X76), d$X83, d$X76)
d$NoAns <- ifelse(is.na(d$X77), d$X84, d$X77)
d$ethnicity <- rowSums(d[,c(2:5,10:11)], na.rm=TRUE)
table(d$ethnicity)/sum(table(d$ethnicity))

# Bar plot
par(mar = c(7, 4, 2, 2) + 0.9)
bar.pre <- barplot(c(1, 11, 1, 3, 2, 2, 23, 4), 
               ylab="Frequency", ylim=c(0,30),
               names.arg=c("American Indian or\nAlaskan Native",
                           "Asian or\nPacific Islander", 
                           "Asian or Pacific Islander &\nWhite/Caucasian",
                           "Black or\nAfrican American",
                           "Hispanic or\nLatino",
                           "Hispanic or Latino &\nWhite/Caucasian", 
                           "White/\nCaucasian",
                           "Prefer not to\nanswer/NA"),
               cex.names=0.7, cex.axis=0.8, las=2)
text(x = bar.pre, 
     y = c(1, 11, 1, 3, 2, 2, 23, 4), 
     label = c(1, 11, 1, 3, 2, 2, 23, 4), 
     pos = 3, cex = 0.8, col = "black")

par(mar = c(7, 4, 2, 2) + 0.9)
bar.pre <- barplot(c(1, 9, 1, 3, 1, 1, 24, 6), 
                   ylab="Frequency", ylim=c(0,30),
                   names.arg=c("American Indian or\nAlaskan Native",
                               "Asian or\nPacific Islander", 
                               "Asian or Pacific Islander &\nWhite/Caucasian",
                               "Black or\nAfrican American",
                               "Hispanic or\nLatino",
                               "Hispanic or Latino &\nWhite/Caucasian", 
                               "White/\nCaucasian",
                               "Prefer not to\nanswer/NA"),
                   cex.names=0.7, cex.axis=0.8, las=2)
text(x = bar.pre, 
     y = c(1, 9, 1, 3, 1, 1, 24, 6), 
     label = c(1, 9, 1, 3, 1, 1, 24, 6), 
     pos = 3, cex = 0.8, col = "black")

# Degree 
# ~~~~~~

# Frequency table of unique responses
table(Jun16.NFW.pre$X85)/sum(table(Jun16.NFW.pre$X85))
table(Jun16.NFW.pre$X86)/sum(table(Jun16.NFW.pre$X86))

d <- merge(Jun16.NFW.pre[,c(9,77:78)], Jun16.NFW.post[,c(9,70:71)], by="Email", all.y=TRUE)
d$Ugrad <- ifelse(is.na(d$X78), d$X85, d$X78)
d$Grad <- ifelse(is.na(d$X79), d$X86, d$X79)
table(d$Ugrad)/sum(table(d$Ugrad))
table(d$Grad)/sum(table(d$Grad))

# Barplot of degree information
bar <- barplot(matrix(c(1,0,9,1,5,3,29,40,2,0),nr=2), 
               beside=T, 
               col=c("white","gray50"), 
               names.arg=c("Africa", "Asia", "Europe", 
                           "North\nAmerica", "South\nAmerica"),
               ylab="Frequency", ylim=c(0,45),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = matrix(c(1,0,9,1,5,3,29,40,2,0),nr=2), 
     label = matrix(c(1,0,9,1,5,3,29,40,2,0),nr=2), 
     pos = 3, cex = 0.8, col = "black")
legend(1, 40, 
       legend=c("Undergraduate degree \n(Bachelor's or equivalent)","Doctoral degree"), 
       col=c("gray10","gray50"),
       fill=c("white","gray50"),
       border="black",
       bty="n", y.intersp=0.5, cex = 0.75)

bar <- barplot(matrix(c(1,0,8,1,4,5,4,3,24,32,1,0),nr=2), 
               beside=T, 
               col=c("white","gray50"), 
               names.arg=c("Africa", "Asia", "Australia", "Europe", 
                           "North\nAmerica", "South\nAmerica"),
               ylab="Frequency", ylim=c(0,45),
               cex.names=0.8, cex.axis=0.8)
text(x = bar, 
     y = matrix(c(1,0,8,1,4,5,4,3,24,32,1,0),nr=2), 
     label = matrix(c(1,0,8,1,4,5,4,3,24,32,1,0),nr=2), 
     pos = 3, cex = 0.8, col = "black")
legend(1, 40, 
       legend=c("Undergraduate degree \n(Bachelor's or equivalent)","Doctoral degree"), 
       col=c("gray10","gray50"),
       fill=c("white","gray50"),
       border="black",
       bty="n", y.intersp=0.5, cex = 0.75)

# Matched Sample 
# ~~~~~~~~~~~~~~

Jun16.NFW.pp$Gender <- ifelse(is.na(Jun16.NFW.pp$X71.y), Jun16.NFW.pp$X78.x, Jun16.NFW.pp$X71.y)
table(Jun16.NFW.pp$Gender)/sum(table(Jun16.NFW.pp$Gender))

Jun16.NFW.pp$Institution <- ifelse(is.na(Jun16.NFW.pp$X68.y), Jun16.NFW.pp$X14.x, Jun16.NFW.pp$X68.y)
table(Jun16.NFW.pp$Institution)/sum(table(Jun16.NFW.pp$Institution))

Jun16.NFW.pp$White <- ifelse(is.na(Jun16.NFW.pp$X76.y), Jun16.NFW.pp$X83, Jun16.NFW.pp$X76.y)
Jun16.NFW.pp$NoAns <- ifelse(is.na(Jun16.NFW.pp$X77.y), Jun16.NFW.pp$X84, Jun16.NFW.pp$X77.y)
Jun16.NFW.pp$Ethnicity <- rowSums(Jun16.NFW.pp[,c(71:74,156:157)],na.rm=TRUE)
table(Jun16.NFW.pp$Ethnicity)/sum(table(Jun16.NFW.pp$Ethnicity))

Jun16.NFW.pp$Ugrad <- ifelse(is.na(Jun16.NFW.pp$X78.y), Jun16.NFW.pp$X85, Jun16.NFW.pp$X78.y)
Jun16.NFW.pp$Grad <- ifelse(is.na(Jun16.NFW.pp$X79.y), Jun16.NFW.pp$X86, Jun16.NFW.pp$X79.y)
table(Jun16.NFW.pp$Ugrad)/sum(table(Jun16.NFW.pp$Ugrad))
table(Jun16.NFW.pp$Grad)/sum(table(Jun16.NFW.pp$Grad))

