 getwd()
setwd("~/Desktop/204A")
datawide<-read.csv("HW7Data.csv")
id <- (1:100) 
datawide <- cbind(id, datawide)
melt<-reshape(datawide,varying=c("t1","t2","t3"),v.names="confidence",timevar="obs",times=c("t1","t2","t3"),direction="long")
datalong<-melt[order(melt$id),]
datalong[1:10,]
time<-rep(c(1,2,3),100)
datalong<-cbind(datalong,time)
head(datalong)
tail(datalong)
summary(aov(confidence~public + Error(id), data=datalong))
pairwise.t.test(datalong$confidence, datalong$public,
                p.adjust.method = c("bonferroni"))
                
summary(aov(confidence~obs + Error(id), data = datalong)) 
contr.poly(4)
datalong[,6]<-contr.poly(4)[,1]
datalong[,7] <- contr.poly(4)[,2] 
datalong[,8] <- contr.poly(4)[,3] 
colnames(datalong) <- c(colnames(datalong[,1:5]),c("l","q","c"))
head(datalong)
tail(datalong)
summary(aov(confidence ~ l + q + c + Error(id), data = datalong))
summary(aov(confidence ~ l*public + q*public + c*public + 
                 Error(id), data = datalong))
                 
summary(aov(confidence ~ public*obs + Error(id), data = datalong)) 
summary(aov(confidence ~ obs + Error(id), datalong[which(datalong$public == 0),]))
summary(aov(confidence ~ obs + Error(id), datalong[which(datalong$public == 1),]))

pairwise.t.test(datalong[which(datalong$public == 1),"confidence"],
                datalong[which(datalong$public == 1),"obs"],
                paired = TRUE, p.adjust = c("bonf"))
pairwise.t.test(datalong[which(datalong$public == 0),"confidence"],
                datalong[which(datalong$public == 0),"obs"],
                paired = TRUE, p.adjust = c("bonf"))

########                
privatel <- sapply(datawide[which(datawide$public == 0), 2:4], quantile, .025)
privateu <- sapply(datawide[which(datawide$public == 0), 2:4], quantile, .975)
publicl <- sapply(datawide[which(datawide$public == 1), 2:4], quantile, .025)
publicu <- sapply(datawide[which(datawide$public == 1), 2:4], quantile, .975)
with(datalong, interaction.plot(obs, public, confidence, col = c('red', 'dark green'),
                             lwd = c(3, 3), legend = FALSE, ylab = 'Confidence',
                             main = 'Plot of Confidence Scores by Type of School across Time',
                             xlab = 'Time', ylim = c(0, 7)))
lines(privatel, col = 'red', lty = 3, lwd = 2)
lines(privateu, col = 'red', lty = 3, lwd = 2)
lines(publicl, col = 'dark green', lty = 3, lwd = 2)
lines(publicu, col = 'dark green', lty = 3, lwd = 2)
legend('bottom', c('Private', 'Public'), col = c('red', 'dark green'), 
       lwd = c(3, 3), bty = 'n', lty = c(2, 1), horiz = TRUE)
                
