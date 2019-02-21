getwd()
setwd("~/Desktop/204A")
mydata<-read.csv("tempmood_Final.csv")
summary(mydata)
sd(mydata$temp)
sd(mydata$part1)
sd(mydata$part2)
sd(mydata$part3)
sd(mydata$part4)
library(MASS); library(car); library(lavaan)
mydata$combo<-(mydata$part1+mydata$part2+mydata$part3+mydata$part4)/4
head(mydata)
cor.test(mydata$temp,mydata$combo)
cov(mydata$temp,mydata$combo)
#Regression for all participants
summary(lm(combo~temp,data=mydata))
#Regression for part1
summary(lm(part1~temp,data=mydata))
#part2
summary(lm(part2~temp,data=mydata))
#part3
summary(lm(part3~temp,data=mydata))
#part4
summary(lm(part4~temp,data=mydata))

par(mfrow=c(2,2))
plot(mydata$part1~mydata$temp, main="Mood for Participant 1", xlab='Temperature', ylab='Mood', xlim=c(70,90), ylim=c(0,11))
abline(h=mean(mydata$part1), col='red', lty=2)

plot(mydata$part2~mydata$temp, main="Mood for Participant 2", xlab='Temperature', ylab='Mood', xlim=c(70,90), ylim=c(0,11))
abline(h=mean(mydata$part2), col='red', lty=2)

plot(mydata$part3~mydata$temp, main="Mood for Participant 3", xlab='Temperature', ylab='Mood', xlim=c(70,90), ylim=c(0,11))
abline(h=mean(mydata$part3), col='red', lty=2)

plot(mydata$part4~mydata$temp, main="Mood for Participant 4", xlab='Temperature', ylab='Mood', xlim=c(70,90), ylim=c(0,11))
abline(h=mean(mydata$part4), col='red', lty=2)


plot(mydata$combo~mydata$temp, main="Average Mood for All Participants", xlab='Temperature', ylab='Mood', xlim=c(70,90), ylim=c(0,11))
abline(h=mean(mydata$combo), col='red', lty=2)

getwd()
setwd("~/Desktop/204A")
d<-read.csv("socialacceptance_Final.csv")

by(d$psa, d$sports, mean)
by(d$psa, d$female, mean)
by(d$psa, d$sports, sd)
by(d$psa, d$female, sd)
by(d$psa, list(d$sports, d$female), mean)
summary(d)

anova(lm(d$psa ~ d$female + d$sports))

anova(lm(d$psa ~ d$female*d$sports))

psaf <- d$psa[which(d$female == 1)]
sportsf <- d$sports[which(d$female == 1)]

psam <- d$psa[which(d$female == 0)]
sportsm <- d$sports[which(d$female == 0)]

anova(lm(psaf ~ sportsf))
anova(lm(psam ~ sportsm))

pairwise.t.test(psaf, sportsf, p.adjust = c("bonf"))
by(psaf, sportsf, mean)

pairwise.t.test(psam, sportsm, p.adjust=c("bonf"))
by(psam, sportsm, mean)

Sex = factor(d$female)
interaction.plot(factor(d$sports), Sex, d$psa, 
                 type = 'b', leg.bty = "o", leg.bg = "gray", lwd = 2,
                 pch = c(18, 24), xlab = 'Extracurricular Activity Type (1=sports, 0=other)', ylab = 'PSA Score',
                 main = 'Interaction Plot of Extracurricular Activities*Biologial Sex')





