setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------------------
# 1. NFW Data 
#---------------------------------------------------------------------------------------

# June 2015

colnames(Jun15.NFW.pp)[c(28,86,110,116:117)] <- c("TchExp", "TaughtCourse", "Gender", "Ugrad", "Grad")
Jun15.NFW.pp$ClassTime <- NA
Jun15.NFW.pp$Institution <- NA
Jun15.NFW.pp$Ethnicity <- rowSums(Jun15.NFW.pp[,111:115],na.rm=TRUE)
colnames(Jun15.NFW.pp)[87:109] <- c("PIPS.C2.1", "PIPS.C1.1", "PIPS.C2.2", "PIPS.C1.2", 
                                    "PIPS.C2.3", "PIPS.C1.3", "PIPS.C1.4", "PIPS.C1.5",
                                    "PIPS.C1.6", "PIPS.C1.7", "PIPS.C2.4", "PIPS.C1.8",
                                    "PIPS.C1.9", "PIPS.C1.10", "PIPS.C1.11", "PIPS.C1.12",
                                    "PIPS.C2.5", "PIPS.C1.13", "PIPS.C1.14", 
                                    "PIPS.C2.6", "PIPS.C2.7", "PIPS.C2.8", "PIPS.C2.9")
Jun15.NFW.pp$PIPS.C1.15 <- NA
Jun15.NFW.pp$`ThinkPairShare/PeerInstruction` <- NA
colnames(Jun15.NFW.pp)[c(45:46,48:54,56)] <- c("Just-in-TimeTeaching", 
                                   "SCALE-UP/StudioClassroom", 
                                   "InteractiveLectureDemonstrations", 
                                   "CooperativeGroupProblemSolving",
                                   "PhETInteractiveSimulations", 
                                   "OpenSourcePhysics", 
                                   "RankingTasks", 
                                   "RealTimePhysics", 
                                   "TutorialsinIntroductoryPhysics",
                                   "ConceptTests/Inventories")
Jun15.NFW.pp$`LectureTutorialsforIntroductoryAstronomy` <- NA
Jun15.NFW.pp$Know.Pre <- rowSums(Jun15.NFW.pp[,10:13],na.rm=TRUE)
Jun15.NFW.pp$Skill.Pre <- rowSums(Jun15.NFW.pp[,14:17],na.rm=TRUE)
   Jun15.NFW.pp$Skill.Pre <- ifelse(Jun15.NFW.pp$Skill.Pre==0, NA, Jun15.NFW.pp$Skill.Pre)
Jun15.NFW.pp$Eff.Pre <- rowSums(Jun15.NFW.pp[,18:22],na.rm=TRUE)
   Jun15.NFW.pp$Eff.Pre <- ifelse(Jun15.NFW.pp$Eff.Pre==7, 4, Jun15.NFW.pp$Eff.Pre)
Jun15.NFW.pp$Mot.Pre <- rowSums(Jun15.NFW.pp[,23:27],na.rm=TRUE)
Jun15.NFW.pp$SuppOth.Pre <- NA
Jun15.NFW.pp$SuppCol.Pre <- NA
Jun15.NFW.pp$Eval.Pre <- NA
colnames(Jun15.NFW.pp)[171:174] <- c("Know.Post", "Skill.Post", "Eff.Post", "Mot.Post")
Jun15.NFW.pp$SuppOth.Post <- NA
Jun15.NFW.pp$SuppCol.Post <- NA
Jun15.NFW.pp$Eval.Post <- NA
Jun15.NFW.pp[,c(185:186,173:174)] <- Jun15.NFW.pp[,c(185:186,173:174)] - 1 # Recode Responses
colnames(Jun15.NFW.pp)[128:129] <- c("TheOverallQualityoftheWorkshopExceeded...",
                                     "I gained a broad perspective...")
J15 <- Jun15.NFW.pp[,c(1,28,45:46,48:54,56,86,87:109,110,116:117,128:129,171:174,177:192)]
J15$Cohort <- "June2015"
J15 <- J15[,c(1,52:58,50,3:11,51,12,14:32,49,33:36,42:45,59:61,40:41,37,47:48,38:39,2,13,46,62)]

# November 2015

colnames(Nov15.NFW.pp)[c(10,86,111:112,118:119)] <- c("TchExp", "TaughtCourse", "ClassTime", "Gender", "Ugrad", "Grad" )
Nov15.NFW.pp$Institution <- NA
Nov15.NFW.pp$Ethnicity <- rowSums(Nov15.NFW.pp[,113:117],na.rm=TRUE)
colnames(Nov15.NFW.pp)[87:110] <- c("PIPS.C2.1", "PIPS.C1.1", "PIPS.C2.2", "PIPS.C1.2", 
                                   "PIPS.C2.3", "PIPS.C1.3", "PIPS.C1.4", "PIPS.C1.5",
                                   "PIPS.C1.6", "PIPS.C1.7", "PIPS.C2.4", "PIPS.C1.8",
                                   "PIPS.C1.9", "PIPS.C1.10", "PIPS.C1.11", "PIPS.C1.12",
                                   "PIPS.C2.5", "PIPS.C1.13", "PIPS.C1.14", "PIPS.C1.15",
                                   "PIPS.C2.6", "PIPS.C2.7", "PIPS.C2.8", "PIPS.C2.9")
Nov15.NFW.pp$`ThinkPairShare/PeerInstruction` <- NA
Nov15.NFW.pp$`Just-in-TimeTeaching` <- NA
Nov15.NFW.pp$`SCALE-UP/StudioClassroom` <- NA
Nov15.NFW.pp$`InteractiveLectureDemonstrations` <- NA
Nov15.NFW.pp$`CooperativeGroupProblemSolving` <- NA
Nov15.NFW.pp$`PhETInteractiveSimulations` <- NA
Nov15.NFW.pp$`Just-in-TimeTeaching` <- NA
Nov15.NFW.pp$`OpenSourcePhysics` <- NA
Nov15.NFW.pp$`RankingTasks` <- NA
Nov15.NFW.pp$`RealTimePhysics` <- NA
Nov15.NFW.pp$`Just-in-TimeTeaching` <- NA
Nov15.NFW.pp$`TutorialsinIntroductoryPhysics` <- NA
Nov15.NFW.pp$`LectureTutorialsforIntroductoryAstronomy` <- NA
Nov15.NFW.pp$`ConceptTests/Inventories` <- NA
colnames(Nov15.NFW.pp)[11:17] <- c("Know.Pre", "Skill.Pre", "Eff.Pre", "Mot.Pre", 
                                   "SuppOth.Pre", "SuppCol.Pre", "Eval.Pre")
colnames(Nov15.NFW.pp)[176:181] <- c("Know.Post", "Skill.Post", "Eff.Post",  
                                   "SuppOth.Post", "SuppCol.Post", "Eval.Post")
Nov15.NFW.pp[,178:181] <- Nov15.NFW.pp[,178:181] - 1 # Recode Responses
Nov15.NFW.pp$Mot.Post <- NA
colnames(Nov15.NFW.pp)[130:131] <- c("TheOverallQualityoftheWorkshopExceeded...",
                                   "I gained a broad perspective...")
N15 <- Nov15.NFW.pp[,c(1,10,11:17,86,87:110,111:112,118:119,130:131,176:181,183:197)]
N15$Cohort <- "Nov2015"
N15 <- N15[,c(1,3:9,49:60,11:34,41:43,61,44:46,39:40,36,47:48,37:38,2,10,35,62)]

# June 2016

Jun16.NFW.pp$Gender <- ifelse(is.na(Jun16.NFW.pp$X71.y), Jun16.NFW.pp$X78.x, Jun16.NFW.pp$X71.y)
Jun16.NFW.pp$Institution <- ifelse(is.na(Jun16.NFW.pp$X68.y), Jun16.NFW.pp$X14.x, Jun16.NFW.pp$X68.y)
Jun16.NFW.pp$White <- ifelse(is.na(Jun16.NFW.pp$X76.y), Jun16.NFW.pp$X83, Jun16.NFW.pp$X76.y)
Jun16.NFW.pp$NoAns <- ifelse(is.na(Jun16.NFW.pp$X77.y), Jun16.NFW.pp$X84, Jun16.NFW.pp$X77.y)
Jun16.NFW.pp$Ethnicity <- rowSums(Jun16.NFW.pp[,c(71:74,156:157)],na.rm=TRUE)
Jun16.NFW.pp$Ugrad <- ifelse(is.na(Jun16.NFW.pp$X78.y), Jun16.NFW.pp$X85, Jun16.NFW.pp$X78.y)
Jun16.NFW.pp$Grad <- ifelse(is.na(Jun16.NFW.pp$X79.y), Jun16.NFW.pp$X86, Jun16.NFW.pp$X79.y)
Jun16.NFW.pp$TchExp <- ifelse(is.na(Jun16.NFW.pp$X69.y), Jun16.NFW.pp$X16.x, Jun16.NFW.pp$X69.y)
Jun16.NFW.pp$TaughtCourse <- Jun16.NFW.pp$X52.x
Jun16.NFW.pp$ClassTime <- ifelse(is.na(Jun16.NFW.pp$X70.y), Jun16.NFW.pp$X53.x, Jun16.NFW.pp$X70.y)
colnames(Jun16.NFW.pp)[46:69] <- c("PIPS.C2.1", "PIPS.C1.1", "PIPS.C2.2", "PIPS.C1.2", 
                                   "PIPS.C2.3", "PIPS.C1.3", "PIPS.C1.4", "PIPS.C1.5",
                                   "PIPS.C1.6", "PIPS.C1.7", "PIPS.C2.4", "PIPS.C1.8",
                                   "PIPS.C1.9", "PIPS.C1.10", "PIPS.C1.11", "PIPS.C1.12",
                                   "PIPS.C2.5", "PIPS.C1.13", "PIPS.C1.14", "PIPS.C1.15",
                                   "PIPS.C2.6", "PIPS.C2.7", "PIPS.C2.8", "PIPS.C2.9")
colnames(Jun16.NFW.pp)[32:43] <- c("ThinkPairShare/PeerInstruction", 
                                   "Just-in-TimeTeaching", 
                                   "SCALE-UP/StudioClassroom", 
                                   "InteractiveLectureDemonstrations", 
                                   "CooperativeGroupProblemSolving",
                                   "PhETInteractiveSimulations", 
                                   "OpenSourcePhysics", 
                                   "RankingTasks", 
                                   "RealTimePhysics", 
                                   "TutorialsinIntroductoryPhysics", 
                                   "LectureTutorialsforIntroductoryAstronomy",
                                   "ConceptTests/Inventories")
colnames(Jun16.NFW.pp)[12:18] <- c("Know.Pre", "Skill.Pre", "Eff.Pre", "Mot.Pre", 
                                      "SuppOth.Pre", "SuppCol.Pre", "Eval.Pre")
colnames(Jun16.NFW.pp)[90:96] <- c("Know.Post", "Skill.Post", "Eff.Post", "Mot.Post", 
                                      "SuppOth.Post", "SuppCol.Post", "Eval.Post")
colnames(Jun16.NFW.pp)[97:98] <- c("TheOverallQualityoftheWorkshopExceeded...",
                                    "I gained a broad perspective...") 
J16 <- Jun16.NFW.pp[,c(1,12:18,32:43,46:69,90:96,97:98,154:155,158,159:160,161:163)]
J16$Cohort <- "June2016"

rm(Jun15.NFW.pre, Jun15.NFW.post, Jun15.NFW.pp,
   Nov15.NFW.pre, Nov15.NFW.post, Nov15.NFW.pp,
   Jun16.NFW.pre, Jun16.NFW.post, Jun16.NFW.pp)

# Create Year1 Merged Sample

Y1 <- rbind(J15, N15, J16)

# write.csv(Y1,file="Y1_NFW_Matched_Sample.csv", na="", row.names=FALSE)

rm(J15, N15, J16)


