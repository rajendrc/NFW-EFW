setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 EFW Analyses")
source("00_CleanData.R")

#-----------------------------------------------------------------------------------
# 07. Specific workshop sessions.  
#-----------------------------------------------------------------------------------

# Frequencies of each response (including 0 response)
X31 <- table(EFW.post$X31); prop.table(X31); sum(X31);
  X31 <- c("0"=1, 0, 0, "3"=5, "4"=12, "5"=14)
X32 <- table(EFW.post$X32); prop.table(X32); sum(X32);
  X32 <- c(0, 0, "2"=4, "3"=5, "4"=9, "5"=15)
X34 <- table(EFW.post$X34); prop.table(X34); sum(X34);
  X34 <- c(0, table(EFW.post$X34))
X35 <- table(EFW.post$X35); prop.table(X35); sum(X35);
  X35 <- c(0, table(EFW.post$X35))
X36 <- table(EFW.post$X36); prop.table(X36); sum(X36);
  X36 <- c(0, table(EFW.post$X36))
X37 <- table(EFW.post$X37); prop.table(X37); sum(X37)
X38 <- table(EFW.post$X38); prop.table(X38); sum(X38)
X39 <- table(EFW.post$X39); prop.table(X39); sum(X39);
  X39 <- c(0, 0, "2"=5, "3"=8, "4"=18, "5"=2)
X40 <- table(EFW.post$X40); prop.table(X40); sum(X40);
  X40 <- c("0"=1, 0, "2"=2, "3"=2, "4"=11, "5"=17)
X41 <- table(EFW.post$X41); prop.table(X41); sum(X41)
X43 <- table(EFW.post$X43); prop.table(X43); sum(X43)
X44 <- table(EFW.post$X44); prop.table(X44); sum(X44);
  X44 <- c(0, table(EFW.post$X44))
X45 <- table(EFW.post$X45); prop.table(X45); sum(X45)

# Barplots
barplot(X31, ylab="Frequency", ylim=c(0,30))
barplot(X32, ylab="Frequency", ylim=c(0,30))
barplot(X34, ylab="Frequency", ylim=c(0,30))
barplot(X35, ylab="Frequency", ylim=c(0,30))
barplot(X36, ylab="Frequency", ylim=c(0,30))
barplot(X37, ylab="Frequency", ylim=c(0,30))
barplot(X38, ylab="Frequency", ylim=c(0,30))
barplot(X39, ylab="Frequency", ylim=c(0,30))
barplot(X40, ylab="Frequency", ylim=c(0,30))
barplot(X41, ylab="Frequency", ylim=c(0,30))
barplot(X43, ylab="Frequency", ylim=c(0,30))
barplot(X44, ylab="Frequency", ylim=c(0,30))
barplot(X45, ylab="Frequency", ylim=c(0,30))

# Means, medians, p-values
EFW.post[,c(27:28, 30:37, 39:41)][EFW.post[,c(27:28, 30:37, 39:41)]==0] <- NA  
describe(EFW.post[,c(27:28, 30:37, 39:41)])
EFW.post[,c(27:28, 30:37, 39:41)]<-EFW.post[,c(27:28, 30:37, 39:41)]/5
describe(EFW.post[,c(27:28, 30:37, 39:41)])



X32 <- c(0, 0, table(NFW$X32)); table(NFW$X32)/sum(table(NFW$X32))
X33 <- table(NFW$X33); table(NFW$X33)/sum(table(NFW$X33)); X33 <- c("0"=3, 0, "2"=1, "3"=6, "4"=19, "5"=27)
X34 <- table(NFW$X34); table(NFW$X34)/sum(table(NFW$X34))
X35 <- c(0, table(NFW$X35)); table(NFW$X35)/sum(table(NFW$X35))
X36 <- table(NFW$X36); table(NFW$X36)/sum(table(NFW$X36)); X36 <- c("0"=1, 0, "2"=1, "3"=9, "4"=30, "5"=15)
X37 <- table(NFW$X37); table(NFW$X37)/sum(table(NFW$X37))
X38 <- table(NFW$X38); table(NFW$X38)/sum(table(NFW$X38)); X38 <- c("0"=3, 0, "2"=4, "3"=7, "4"=25, "5"=17)
X39 <- table(NFW$X39); table(NFW$X39)/sum(table(NFW$X39)); X39 <- c("0"=37, 0, "2"=1, "3"=3, "4"=6, "5"=8)
X40 <- table(NFW$X40); table(NFW$X40)/sum(table(NFW$X40))
X41 <- table(NFW$X41); table(NFW$X41)/sum(table(NFW$X41))
X42 <- table(NFW$X42); table(NFW$X42)/sum(table(NFW$X42))
X43 <- table(NFW$X43); table(NFW$X43)/sum(table(NFW$X43))
X44 <- table(NFW$X44); table(NFW$X44)/sum(table(NFW$X44))
X45 <- table(NFW$X45); table(NFW$X45)/sum(table(NFW$X45))
X46 <- table(NFW$X46); table(NFW$X46)/sum(table(NFW$X46))
X47 <- table(NFW$X47); table(NFW$X47)/sum(table(NFW$X47)); X47 <- c("0"=5, 0, "2"=2, "3"=11, "4"=17, "5"=21)
X48 <- table(NFW$X48); table(NFW$X48)/sum(table(NFW$X48))
X49 <- table(NFW$X49); table(NFW$X49)/sum(table(NFW$X49))
X50 <- table(NFW$X50); table(NFW$X50)/sum(table(NFW$X50))

# Recoding 0=NA

NFW[,17:37][NFW[,17:37]==0] <- NA  

X30 <- c(0, 0, table(NFW$X30)); table(NFW$X30)/sum(table(NFW$X30))
X31 <- table(NFW$X31); table(NFW$X31)/sum(table(NFW$X31))
X32 <- c(0, table(NFW$X32)); table(NFW$X32)/sum(table(NFW$X32))
X33 <- c(0, table(NFW$X33)); table(NFW$X33)/sum(table(NFW$X33))
X34 <- table(NFW$X34); table(NFW$X34)/sum(table(NFW$X34))
X35 <- table(NFW$X35); table(NFW$X35)/sum(table(NFW$X35))
X36 <- c(0, table(NFW$X36)); table(NFW$X36)/sum(table(NFW$X36))
X37 <- table(NFW$X37); table(NFW$X37)/sum(table(NFW$X37))
X38 <- c(0, table(NFW$X38)); table(NFW$X38)/sum(table(NFW$X38))
X39 <- c(0, table(NFW$X39)); table(NFW$X39)/sum(table(NFW$X39))
X40 <- table(NFW$X40); table(NFW$X40)/sum(table(NFW$X40))
X41 <- table(NFW$X41); table(NFW$X41)/sum(table(NFW$X41))
X42 <- table(NFW$X42); table(NFW$X42)/sum(table(NFW$X42))
X43 <- table(NFW$X43); table(NFW$X43)/sum(table(NFW$X43))
X44 <- table(NFW$X44); table(NFW$X44)/sum(table(NFW$X44))
X45 <- table(NFW$X45); table(NFW$X45)/sum(table(NFW$X45))
X46 <- table(NFW$X46); table(NFW$X46)/sum(table(NFW$X46))
X47 <- c(0, table(NFW$X47)); table(NFW$X47)/sum(table(NFW$X47))
X48 <- table(NFW$X48); table(NFW$X48)/sum(table(NFW$X48))
X49 <- table(NFW$X49); table(NFW$X49)/sum(table(NFW$X49))
X50 <- table(NFW$X50); table(NFW$X50)/sum(table(NFW$X50))

# Bar plots

barplot(X30, ylab="Frequency", ylim=c(0,60))
barplot(X31, ylab="Frequency", ylim=c(0,60))
barplot(X32, ylab="Frequency", ylim=c(0,60))
barplot(X33, ylab="Frequency", ylim=c(0,60))
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
barplot(X44, ylab="Frequency", ylim=c(0,60))
barplot(X45, ylab="Frequency", ylim=c(0,60))
barplot(X46, ylab="Frequency", ylim=c(0,60))
barplot(X47, ylab="Frequency", ylim=c(0,60))
barplot(X48, ylab="Frequency", ylim=c(0,60))
barplot(X49, ylab="Frequency", ylim=c(0,60))
barplot(X50, ylab="Frequency", ylim=c(0,60))

# For n, mean, median, sd, and p-values 

describe(NFW[,17:37])

NFW[,17:37]<-NFW[,17:37]/5
describe(NFW[,17:37])
