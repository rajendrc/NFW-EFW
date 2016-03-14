setwd("~/Dropbox/EFW Analyses/Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 08. Appendix
#---------------------------------------------------------------------------

# Sample size, means, medians
describe(EFW[,c(51:74)])

# Barplots
barplot(c(0, table(EFW[,51])), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,52]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,53]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,54]), ylab="Frequency", ylim=c(0,40))
barplot(c(0, table(EFW[,55])), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,56]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,57]), ylab="Frequency", ylim=c(0,40))
barplot(c(0, table(EFW[,58])), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,59]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,60]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,61]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,62]), ylab="Frequency", ylim=c(0,40))
barplot(c(table(EFW[,63]),0), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,64]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,65]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,66]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,67]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,68]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,69]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,70]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,71]), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,72]), ylab="Frequency", ylim=c(0,40))
barplot(c(0, table(EFW[,73])), ylab="Frequency", ylim=c(0,40))
barplot(table(EFW[,74]), ylab="Frequency", ylim=c(0,40))

# For p-values
EFW[,c(51:74)] <- EFW[,c(51:74)]/5
describe(EFW[,c(51:74)])
