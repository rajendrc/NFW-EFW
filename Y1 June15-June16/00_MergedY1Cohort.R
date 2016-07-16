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
Nov15.NFW.pp$ClassTime <- ifelse(Nov15.NFW.pp$ClassTime==1 | Nov15.NFW.pp$ClassTime==2, 1, Nov15.NFW.pp$ClassTime)
Nov15.NFW.pp$ClassTime <- ifelse(Nov15.NFW.pp$ClassTime==3, 2, Nov15.NFW.pp$ClassTime)
Nov15.NFW.pp$ClassTime <- ifelse(Nov15.NFW.pp$ClassTime==4 | Nov15.NFW.pp$ClassTime==5, 3, Nov15.NFW.pp$ClassTime)
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
Jun16.NFW.pp$ClassTime <- ifelse(Jun16.NFW.pp$ClassTime<=10, 1, Jun16.NFW.pp$ClassTime)
Jun16.NFW.pp$ClassTime <- ifelse(Jun16.NFW.pp$ClassTime>10 & Jun16.NFW.pp$ClassTime<=20, 2, Jun16.NFW.pp$ClassTime)
Jun16.NFW.pp$ClassTime <- ifelse(Jun16.NFW.pp$ClassTime>20, 3, Jun16.NFW.pp$ClassTime)
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

#---------------------------------------------------------------------------------------
# 2. EFW Data 
#---------------------------------------------------------------------------------------

# March 2016 

setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 EFW Analyses")
source("00_CleanData.R")

colnames(EFW.pp)[c(14,11,15,75,80:81)] <- c("TchExp", "Institution", "ClassTime", "Gender", "Ugrad", "Grad" )
EFW.pp$ClassTime <- ifelse(EFW.pp$ClassTime<=10, 1, EFW.pp$ClassTime)
EFW.pp$ClassTime <- ifelse(EFW.pp$ClassTime>10 & EFW.pp$ClassTime<=20, 2, EFW.pp$ClassTime)
EFW.pp$ClassTime <- ifelse(EFW.pp$ClassTime>20, 3, EFW.pp$ClassTime)
EFW.pp$TaughtCourse <- NA
EFW.pp$Ethnicity <- rowSums(EFW.pp[,76:79],na.rm=TRUE)
colnames(EFW.pp)[51:74] <- c("PIPS.C2.1", "PIPS.C1.1", "PIPS.C2.2", "PIPS.C1.2", 
                                    "PIPS.C2.3", "PIPS.C1.3", "PIPS.C1.4", "PIPS.C1.5",
                                    "PIPS.C1.6", "PIPS.C1.7", "PIPS.C2.4", "PIPS.C1.8",
                                    "PIPS.C1.9", "PIPS.C1.10", "PIPS.C1.11", "PIPS.C1.12",
                                    "PIPS.C2.5", "PIPS.C1.13", "PIPS.C1.14", "PIPS.C1.15",
                                    "PIPS.C2.6", "PIPS.C2.7", "PIPS.C2.8", "PIPS.C2.9")
colnames(EFW.pp)[c(26:29,32:38,41)] <- c("ThinkPairShare/PeerInstruction", 
                                   "Just-in-TimeTeaching", 
                                   "SCALE-UP/StudioClassroom", 
                                 "LectureTutorialsforIntroductoryAstronomy",
                                   "InteractiveLectureDemonstrations", 
                                   "CooperativeGroupProblemSolving",
                                   "PhETInteractiveSimulations", 
                                   "OpenSourcePhysics", 
                                   "RankingTasks", 
                                   "RealTimePhysics", 
                                   "TutorialsinIntroductoryPhysics", 
                                   "ConceptTests/Inventories")
colnames(EFW.pp)[16:22] <- c("Know.Pre", "Skill.Pre", "Eff.Pre", "Mot.Pre", 
                                   "SuppOth.Pre", "SuppCol.Pre", "Eval.Pre")
colnames(EFW.pp)[129:135] <- c("Know.Post", "Skill.Post", "Eff.Post", "Mot.Post", 
                                     "SuppOth.Post", "SuppCol.Post", "Eval.Post")
EFW.pp[,131:135] <- EFW.pp[,131:135] - 1 # Recode Responses
colnames(EFW.pp)[92:93] <- c("TheOverallQualityoftheWorkshopExceeded...",
                                     "I gained a broad perspective...")
M16 <- EFW.pp[,c(1,14,11,15,75,80:81,51:74,26:29,32:38,41,16:22,92:93,129:135,148:149)]
M16$Cohort <- "March2015"
M16 <- M16[,c(1,44:50,32:34,36:42,35,43,8:31,53:59,51:52,5,3,61,6:7,2,60,4,62)]

#---------------------------------------------------------------------------------------
# 3. Create Year1 Merged Sample
#---------------------------------------------------------------------------------------

Y1 <- rbind(J15, N15, M16, J16)

rm(Jun15.NFW.pre, Jun15.NFW.post, Jun15.NFW.pp,
   Nov15.NFW.pre, Nov15.NFW.post, Nov15.NFW.pp,
   EFW.pre, EFW.post, EFW.pp,
   Jun16.NFW.pre, Jun16.NFW.post, Jun16.NFW.pp,
   J15, N15, M16, J16)

#---------------------------------------------------------------------------------------
# 4. Clean Year1 Merged Sample
#---------------------------------------------------------------------------------------

# PIPS Total Scores

# Convert any NAs to Os
Y1[,c(21:44)][is.na(Y1[,c(21:44)])] <- 0

# Create a new variable for the total score for both constructs
Y1$PIPS.C1 <- Y1$PIPS.C1.1 + Y1$PIPS.C1.2 + Y1$PIPS.C1.3 + Y1$PIPS.C1.4 +
  Y1$PIPS.C1.5 + Y1$PIPS.C1.6 + Y1$PIPS.C1.7 + Y1$PIPS.C1.8 +
  Y1$PIPS.C1.9 + Y1$PIPS.C1.10 + Y1$PIPS.C1.11 + Y1$PIPS.C1.12 +
  Y1$PIPS.C1.13 + Y1$PIPS.C1.14 + Y1$PIPS.C1.15
Y1$PIPS.C1 <- (Y1$PIPS.C1/75)*100
Y1$PIPS.C2 <- Y1$PIPS.C2.1 + Y1$PIPS.C2.2 + Y1$PIPS.C2.3 + Y1$PIPS.C2.4 + 
  Y1$PIPS.C2.5 + Y1$PIPS.C2.6 + Y1$PIPS.C2.7 + Y1$PIPS.C2.8 + 
  Y1$PIPS.C2.9
Y1$PIPS.C2 <- (Y1$PIPS.C2/45)*100

# Remove non-respondents
Y1[,63:64][Y1[,63:64]==0] <- NA  

# Remove Unnecessary Variables
Y1 <- Y1[,c(1,62,2:8,45:51,54,56,57:59,61,63:64)]
Y1.b <- Y1[,c(1,62,2:8,45:51,52:53,54,56,57:59,61,63:64)]

# Recode all 0 responses to NA
Y1[,3:16][Y1[,3:16]==0] <- NA  

# Compute Gain for each Common Item
Y1$Know.Gain <- Y1$Know.Post - Y1$Know.Pre
Y1$Skill.Gain <- Y1$Skill.Post - Y1$Skill.Pre
Y1$Eff.Gain <- Y1$Eff.Post - Y1$Eff.Pre
Y1$Mot.Gain <- Y1$Mot.Post - Y1$Mot.Pre
Y1$SuppOth.Gain <- Y1$SuppOth.Post - Y1$SuppOth.Pre
Y1$SuppCol.Gain <- Y1$SuppCol.Post - Y1$SuppCol.Pre
Y1$Eval.Gain <- Y1$Eval.Post - Y1$Eval.Pre

#---------------------------------------------------------------------------------------
# 5. Write to csv
#---------------------------------------------------------------------------------------

# write.csv(Y1,file="Y1_NFW_Matched_Sample.csv", na="", row.names=FALSE)
