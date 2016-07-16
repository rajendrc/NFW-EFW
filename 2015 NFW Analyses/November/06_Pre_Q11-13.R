#-----------------------------------------------------------------------------------
# Load  Data File
#-----------------------------------------------------------------------------------

setwd("~/Documents/Rajendra Chattergoon/CU Boulder/Consulting/Stephanie Chasteen/2015-16 NFW/1516 NFW Survey")
d<-read.csv("2011_11_NFW_Pre-Survey_Q11-13.csv")

#-----------------------------------------------------------------------------------
# Analysis - Q11-Q13
#-----------------------------------------------------------------------------------

install.packages(sjPlot)
library(sjPlot)

sjp.setTheme(theme="theme_bw", title.align="center",
             title.size=1.8,
             axis.title.size=1.5,
             geom.label.size=5,
             legend.size=1,
             axis.textcolor.x="Black",
             axis.textcolor.y="Black")

sjp.likert(d[, 2:13], 
           catcount = 6, 
           legendLabels=c("I have been in a class (as a student) which used this technique",
                          "I have used this technique as a teacher (currently or in the past)", 
                          "I have heard of this technique",
                          "I have never heard of it / Not Sure",
                          " ",
                          " "),
           sort.frq="neg.desc",
           value.labels="sum.outside",
           geom.colors="Greys",
           showPercentageSign=TRUE,
           reverse.colors=TRUE,
           expand.grid=TRUE,
           includeN=TRUE,
           axisLabels.y=c(
             "11.a. Think-Pair-Share",
             "11.b. Peer Instruction",
             "11.c. Just-in-time Teaching",
             "11.d. SCALE-UP Classroom", 
             "12.a. Interactive Lecture Demonstrations",
             "12.b. Cooperative group problem solving",
             "12.c. PhET Interactive Simulations",
             "12.d. Open Source Physics",
             "12.e. Ranking tasks",
             "12.f. Real Time Physics",
             "12.g. Tutorials in Introductory Physics",
             "13.a. Concept Tests/Inventories"
             ),
           gridRange=1.1,
           title="Frequency Distribution of Responses"
) 




