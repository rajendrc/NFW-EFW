setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#-----------------------------------------------------------------------------------
# 08. Specific workshop sessions.  
#-----------------------------------------------------------------------------------

# Frequencies of each response (including 0 response)
X37 <- table(Jun16.NFW.post$X37); prop.table(X37); sum(X37);
  X37 <- c("0"=2, 0, 0, "3"=3, "4"=19, "5"=21)
X38 <- table(Jun16.NFW.post$X38); prop.table(X38); sum(X38);
  X38 <- c("0"=3, 0, "2"=2, "3"=8, "4"=17, "5"=13)
X39 <- table(Jun16.NFW.post$X39); prop.table(X39); sum(X39);
  X39 <- c("0"=2, 0, "2"=4, "3"=6, "4"=16, "5"=17)  
X40 <- table(Jun16.NFW.post$X40); prop.table(X40); sum(X40)
X41 <- table(Jun16.NFW.post$X41); prop.table(X41); sum(X41)
X42 <- table(Jun16.NFW.post$X42); prop.table(X42); sum(X42)
X43 <- table(Jun16.NFW.post$X43); prop.table(X43); sum(X43);
  X43 <- c(0, 0, 0, "3"=6, "4"=16, "5"=22)
X44 <- table(Jun16.NFW.post$X44); prop.table(X44); sum(X44);
  X44 <- c(0, 0, "2"=3, "3"=10, "4"=17, "5"=15)
X45 <- table(Jun16.NFW.post$X45); prop.table(X45); sum(X45)
X46 <- table(Jun16.NFW.post$X46); prop.table(X46); sum(X46);
  X46 <- c("0"=24, 0, "2"=1, "3"=6, "4"=8, "5"=4)
X47 <- table(Jun16.NFW.post$X47); prop.table(X47); sum(X47);
  X47 <- c("0"=4, 0, "2"=2, "3"=5, "4"=18, "5"=16)
X48 <- table(Jun16.NFW.post$X48); prop.table(X48); sum(X48);
  X48 <- c("0"=3, 0, "2"=4, "3"=8, "4"=12, "5"=18)
X49 <- table(Jun16.NFW.post$X49); prop.table(X49); sum(X49);
  X49 <- c("0"=9, "1"=1, 0, "3"=4, "4"=17, "5"=14)
X50 <- table(Jun16.NFW.post$X50); prop.table(X50); sum(X50)
X51 <- table(Jun16.NFW.post$X51); prop.table(X51); sum(X51)
X52 <- table(Jun16.NFW.post$X52); prop.table(X52); sum(X52)
X53 <- table(Jun16.NFW.post$X53); prop.table(X53); sum(X53);
  X53 <- c("0"=6, 0, "2"=2, "3"=8, "4"=18, "5"=11)
X54 <- table(Jun16.NFW.post$X54); prop.table(X54); sum(X54)
  X54 <- c(0, 0, 0, "3"=5, "4"=13, "5"=27)
X55 <- table(Jun16.NFW.post$X55); prop.table(X55); sum(X55)
X56 <- table(Jun16.NFW.post$X56); prop.table(X56); sum(X56)
  X56 <- c("0"=14, 0, "2"=3, "3"=2, "4"=16, "5"=10)
X58 <- table(Jun16.NFW.post$X58); prop.table(X58); sum(X58)
X59 <- table(Jun16.NFW.post$X59); prop.table(X59); sum(X59)
X60 <- table(Jun16.NFW.post$X60); prop.table(X60); sum(X60)
X61 <- table(Jun16.NFW.post$X61); prop.table(X61); sum(X61);
  X61 <- c("0"=14, 0, "2"=7, "3"=9, "4"=9, "5"=6)

# Barplots
barplot(X37, ylab="Frequency", ylim=c(0,30))
barplot(X38, ylab="Frequency", ylim=c(0,30))
barplot(X39, ylab="Frequency", ylim=c(0,30))
barplot(X40, ylab="Frequency", ylim=c(0,30))
barplot(X41, ylab="Frequency", ylim=c(0,30))
barplot(X42, ylab="Frequency", ylim=c(0,30))
barplot(X43, ylab="Frequency", ylim=c(0,30))
barplot(X44, ylab="Frequency", ylim=c(0,30))
barplot(X45, ylab="Frequency", ylim=c(0,30))
barplot(X46, ylab="Frequency", ylim=c(0,30))
barplot(X47, ylab="Frequency", ylim=c(0,30))
barplot(X48, ylab="Frequency", ylim=c(0,30))
barplot(X49, ylab="Frequency", ylim=c(0,30))
barplot(X50, ylab="Frequency", ylim=c(0,30))
barplot(X51, ylab="Frequency", ylim=c(0,30))
barplot(X52, ylab="Frequency", ylim=c(0,30))
barplot(X53, ylab="Frequency", ylim=c(0,30))
barplot(X54, ylab="Frequency", ylim=c(0,30))
barplot(X55, ylab="Frequency", ylim=c(0,30))
barplot(X56, ylab="Frequency", ylim=c(0,30))
barplot(X58, ylab="Frequency", ylim=c(0,30))
barplot(X59, ylab="Frequency", ylim=c(0,30))
barplot(X60, ylab="Frequency", ylim=c(0,30))
barplot(X61, ylab="Frequency", ylim=c(0,30))

# Means, medians, p-values
Jun16.NFW.post[,c(33:52,54:57)][Jun16.NFW.post[,c(33:52,54:57)]==0] <- NA  
describe(Jun16.NFW.post[,c(33:52,54:57)])
Jun16.NFW.post[,c(33:52,54:57)]<-Jun16.NFW.post[,c(33:52,54:57)]/5
describe(Jun16.NFW.post[,c(33:52,54:57)])

  








  
  
X37:X56
X58:X61
