library(readxl)
library(psych)
library(ltm)

#---------------------------------------------------------------------------
# 1. Import and Clean the Raw Data for Nov 2015.
#---------------------------------------------------------------------------

# Set the working directory to the location of the raw excel file. 
setwd("~/Dropbox/NFW-EFW Analyses/Raw Data/June 2015")

# Import the raw data files. 
raw.pre <- read_excel("Pre-Nov2015-numerical.xls", sheet=1, col_names=FALSE, skip=2)
raw.post <- read_excel("Post-Nov2015-numerical.xls", sheet=1, col_names=FALSE, skip=2)

# Rename the merge variable 
colnames(raw.pre)[9] <- "Email"
colnames(raw.post)[9] <- "Email"

# Rename emails of identical individuals with different emails in pre and post
# Use pre-survey email address
raw.post[raw.post$X1=="4060556493",9] <- "kristin.kraemer@kwu.ed"
raw.post[raw.post$X1=="4052814025",9] <- "scottnoble@utulsa.edu"; raw.pre[raw.pre$X1=="3988279044",9] <- "scottnoble@utulsa.edu"
raw.post[raw.post$X1=="4055105323",9] <- "huron.woods@gmail.com"; raw.pre[raw.pre$X1=="3975667789",9] <- "huron.woods@gmail.com"
# 7 people responded to the post-survey, but not the pre-survey (1 person responded twice)
# 13 people responded to the pre-survey, but not the post-survey

# Remove respondents that were not registered for the conference (N/A for June 2015).
Jun15.NFW.pre <- raw.pre
Jun15.NFW.post <- raw.post

# Merge respondent answers from the post-survey
Jun15.NFW.pp <- merge(raw.pre, raw.post, by="Email")

rm(raw.pre, raw.post)

#---------------------------------------------------------------------------
# 2. Import and Clean the Raw Data for Nov 2015.
#---------------------------------------------------------------------------

# Set the working directory to the location of the raw excel file. 
setwd("~/Dropbox/NFW-EFW Analyses/Raw Data/November 2015")

# Import the raw data files. 
raw.pre <- read_excel("pre Workshop survey Nov2015- results.xls", sheet=1, col_names=FALSE, skip=2)
raw.post <- read_excel("NFW-Nov2015-post-allresponses.xls", sheet=1, col_names=FALSE, skip=2)

# Rename the merge variable 
colnames(raw.pre)[9] <- "Email"
colnames(raw.post)[9] <- "Email"

# Rename emails of identical individuals with different emails in pre and post
# Use pre-survey email address
raw.post[raw.post$X1=="4354281723",9] <- "a.estrade@cmich.edu"
raw.post[raw.post$X1=="4353010458",9] <- "blussem@kent.edu"
raw.post[raw.post$X1=="4386426121",9] <- "jyoung35@atu.edu"
raw.post[raw.post$X1=="4368758438",9] <- "Larmendarez@kwc.edu"
raw.post[raw.post$X1=="4381719312",9] <- "Luke.Corwin@sdsmt.edu"
raw.post[raw.post$X1=="4351391551",9] <- "mseifer@conncoll.edu"; raw.pre[raw.pre$X1=="4244251543",9] <- "mseifer@conncoll.edu"
# 17 people responded to the post-survey, but not the pre-survey
# 20 people responded to the pre-survey, but not the post-survey

# Remove respondents that were not registered for the conference (N/A for November 2015).
Nov15.NFW.pre <- raw.pre
Nov15.NFW.post <- raw.post

# Merge respondent answers from the post-survey
Nov15.NFW.pp <- merge(raw.pre, raw.post, by="Email")

rm(raw.pre, raw.post)

#---------------------------------------------------------------------------
# 3. Import and Clean the Raw Data for June 2016.
#---------------------------------------------------------------------------

# Set the working directory to the location of the raw excel file. 
setwd("~/Dropbox/NFW-EFW Analyses/Raw Data/June 2016")

# Import the raw data files. 
raw.pre <- read_excel("numerical-answer.xls", sheet=1, col_names=FALSE, skip=2)
raw.post <- read_excel("JuneNFW-2016-post.xls", sheet=1, col_names=FALSE, skip=2)

# Rename the merge variable 
colnames(raw.pre)[9] <- "Email"
colnames(raw.post)[9] <- "Email"

# Remove duplicate entries from the post-survey
raw.post <- raw.post[raw.post$X1!="4836887699",] 
raw.post <- raw.post[raw.post$X1!="4821819743",] 

# Rename emails of identical individuals with different emails in pre and post
# Use pre-survey email address
raw.post[raw.post$X10=="Avery",9] <- "Averya@msudenver.edu"
# 6 people responded to the post-survey, but not the pre-survey
# 7 people responded to the pre-survey, but not the post-survey

# Remove respondents that were not registered for the conference (N/A for June 2016).
Jun16.NFW.pre <- raw.pre
Jun16.NFW.post <- raw.post
  
# Merge respondent answers from the post-survey
Jun16.NFW.pp <- merge(raw.pre, raw.post, by="Email")

rm(raw.pre, raw.post)

