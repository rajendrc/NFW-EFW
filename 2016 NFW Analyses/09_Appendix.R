setwd("~/Dropbox/NFW-EFW Analyses/NFW-EFW/2016 NFW Analyses")
source("00_CleanData.R")

#---------------------------------------------------------------------------
# 08. Appendix
#---------------------------------------------------------------------------

# Sample size, means, medians
describe(NFW.pre[,c(46:69)])

# Barplots
barplot(table(NFW.pre[,46]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,47]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,48]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,49]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,50]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,51]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,52]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,53]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,54]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,55]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,56]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,57]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,58]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,59]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,60]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,61]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,62]), ylab="Frequency", ylim=c(0,40))
barplot(c(0,table(NFW.pre[,63])), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,64]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,65]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,66]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,67]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,68]), ylab="Frequency", ylim=c(0,40))
barplot(table(NFW.pre[,69]), ylab="Frequency", ylim=c(0,40))

# For p-values
NFW.pre[,c(46:69)] <- NFW.pre[,c(46:69)]/5
describe(NFW.pre[,c(46:69)])
