 getwd()
setwd("~/Desktop/204A")
mydata<-read.csv("204AHW6Data.csv")
summary(mydata)
by(mydata$biosex, mydata$workout, summary)
by(mydata$pctgain, list(mydata$biosex, mydata$workout), mean)
by(mydata$pctgain, list(mydata$biosex, mydata$workout), sd)
by(mydata$pctgain, mydata$biosex, mean)
by(mydata$pctgain, mydata$biosex, sd)
by(mydata$pctgain, mydata$workout, mean)
by(mydata$pctgain, mydata$workout, sd)
anova(lm(mydata$pctgain ~ mydata$biosex + mydata$workout + mydata$biosex*mydata$workout))
anova(lm(mydata$pctgain ~ mydata$biosex*mydata$workout))

mydata$biosexbinary<- 0
mydata$biosexbinary[mydata$biosex == "f"]<-1
pctgainf <- mydata$pctgain[which(mydata$biosexbinary == 1)]
workoutf <- mydata$workout[which(mydata$biosexbinary == 1)]

pctgainm <- mydata$pctgain[which(mydata$biosexbinary == 0)]
workoutm <- mydata$workout[which(mydata$biosexbinary == 0)]

anova(lm(pctgainf ~ workoutf))
anova(lm(pctgainm ~ workoutm))

pairwise.t.test(pctgainf, workoutf, p.adjust = c("bonf"))
by(pctgainf, workoutf, mean)

pairwise.t.test(pctgainm, workoutm, p.adjust=c("bonf"))
by(pctgainm, workoutm, mean)
