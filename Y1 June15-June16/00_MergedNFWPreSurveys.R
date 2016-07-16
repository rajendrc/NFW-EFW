setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------------------
# 1. NFW Data 
#---------------------------------------------------------------------------------------
rm(Jun15.NFW.pp, Jun15.NFW.post, Nov15.NFW.pp, Nov15.NFW.post, Jun16.NFW.pp, Jun16.NFW.post)

# June 2015

colnames(Jun15.NFW.pre)[c(28,86,110,116:117)] <- c("TchExp", "TaughtCourse", "Gender", "Ugrad", "Grad")
Jun15.NFW.pre$ClassTime <- NA
Jun15.NFW.pre$Institution <- NA
Jun15.NFW.pre$Ethnicity <- rowSums(Jun15.NFW.pre[,111:115],na.rm=TRUE)
colnames(Jun15.NFW.pre)[87:109] <- c("PIPS.C2.1", "PIPS.C1.1", "PIPS.C2.2", "PIPS.C1.2", 
                                    "PIPS.C2.3", "PIPS.C1.3", "PIPS.C1.4", "PIPS.C1.5",
                                    "PIPS.C1.6", "PIPS.C1.7", "PIPS.C2.4", "PIPS.C1.8",
                                    "PIPS.C1.9", "PIPS.C1.10", "PIPS.C1.11", "PIPS.C1.12",
                                    "PIPS.C2.5", "PIPS.C1.13", "PIPS.C1.14", 
                                    "PIPS.C2.6", "PIPS.C2.7", "PIPS.C2.8", "PIPS.C2.9")
Jun15.NFW.pre$PIPS.C1.15 <- NA
J15 <- Jun15.NFW.pre[,c(9,28,86,87:109,110,116:117,120:123)]
J15$Cohort <- "June2015"
J15 <- J15[,c(1,31,2:3,30,4:22,33,23:26,27,28:29,32,34)]

# November 2015

colnames(Nov15.NFW.pre)[c(10,86,111:112,118:119)] <- c("TchExp", "TaughtCourse", "ClassTime", "Gender", "Ugrad", "Grad" )
Nov15.NFW.pre$ClassTime <- ifelse(Nov15.NFW.pre$ClassTime==1 | Nov15.NFW.pre$ClassTime==2, 1, Nov15.NFW.pre$ClassTime)
Nov15.NFW.pre$ClassTime <- ifelse(Nov15.NFW.pre$ClassTime==3, 2, Nov15.NFW.pre$ClassTime)
Nov15.NFW.pre$ClassTime <- ifelse(Nov15.NFW.pre$ClassTime==4 | Nov15.NFW.pre$ClassTime==5, 3, Nov15.NFW.pre$ClassTime)
Nov15.NFW.pre$Institution <- NA
Nov15.NFW.pre$Ethnicity <- rowSums(Nov15.NFW.pre[,113:117],na.rm=TRUE)
colnames(Nov15.NFW.pre)[87:110] <- c("PIPS.C2.1", "PIPS.C1.1", "PIPS.C2.2", "PIPS.C1.2", 
                                    "PIPS.C2.3", "PIPS.C1.3", "PIPS.C1.4", "PIPS.C1.5",
                                    "PIPS.C1.6", "PIPS.C1.7", "PIPS.C2.4", "PIPS.C1.8",
                                    "PIPS.C1.9", "PIPS.C1.10", "PIPS.C1.11", "PIPS.C1.12",
                                    "PIPS.C2.5", "PIPS.C1.13", "PIPS.C1.14", "PIPS.C1.15",
                                    "PIPS.C2.6", "PIPS.C2.7", "PIPS.C2.8", "PIPS.C2.9")
N15 <- Nov15.NFW.pre[,c(9,10,86,87:110,111:112,118:119, 122:123)]
N15$Cohort <- "Nov2015"
N15 <- N15[,c(1,32,2:3,28,4:27,29,30:31,33:34)]

# June 2016

colnames(Jun16.NFW.pre)[c(10:11,44,45,70,77:78)] <- c("Institution", "TchExp", "TaughtCourse", "ClassTime", "Gender", "Ugrad", "Grad" )
Jun16.NFW.pre$ClassTime <- ifelse(Jun16.NFW.pre$ClassTime<=10, 1, Jun16.NFW.pre$ClassTime)
Jun16.NFW.pre$ClassTime <- ifelse(Jun16.NFW.pre$ClassTime>10 & Jun16.NFW.pre$ClassTime<=20, 2, Jun16.NFW.pre$ClassTime)
Jun16.NFW.pre$ClassTime <- ifelse(Jun16.NFW.pre$ClassTime>20, 3, Jun16.NFW.pre$ClassTime)
Jun16.NFW.pre$Ethnicity <- rowSums(Jun16.NFW.pre[,71:76],na.rm=TRUE)
colnames(Jun16.NFW.pre)[46:69] <- c("PIPS.C2.1", "PIPS.C1.1", "PIPS.C2.2", "PIPS.C1.2", 
                                   "PIPS.C2.3", "PIPS.C1.3", "PIPS.C1.4", "PIPS.C1.5",
                                   "PIPS.C1.6", "PIPS.C1.7", "PIPS.C2.4", "PIPS.C1.8",
                                   "PIPS.C1.9", "PIPS.C1.10", "PIPS.C1.11", "PIPS.C1.12",
                                   "PIPS.C2.5", "PIPS.C1.13", "PIPS.C1.14", "PIPS.C1.15",
                                   "PIPS.C2.6", "PIPS.C2.7", "PIPS.C2.8", "PIPS.C2.9")
J16 <- Jun16.NFW.pre[,c(9,10:11,44,45,46:69,70,77:78,82)]
J16$Cohort <- "June2016"

# Create Merged Sample of NFW Pre-Surveys

Y1.NFW <- rbind(J15, N15, J16)

# write.csv(Y1.NFW, file="Y1NFW_PreSurveys.csv", na="", row.names=FALSE)

rm(Jun15.NFW.pre, Jun16.NFW.pre, Nov15.NFW.pre, J15, N15, J16)


