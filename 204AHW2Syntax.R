getwd()
setwd("~/Desktop/204A")
mydata<-read.csv("204AHW1Data.csv")
hist((mydata$weight_lbs),xlab="Weight in Pounds",main="Histogram of Weight")
plot(density(mydata$height_in),xlab="Height in Inches",main="Density Plot of Height")
abline(v=mean(mydata$height_in))
abline(v=median(mydata$height_in),lty=2)
mydata$bmi<-((mydata$weight_lbs/(mydata$height_in)^2)*703)
by(mydata$bmi,list(mydata$biosex, mydata$ed_cmplt),mean)
by(mydata$bmi,list(mydata$biosex, mydata$ed_cmplt),median)
by(mydata$bmi,list(mydata$biosex, mydata$ed_cmplt),var)
by(mydata$bmi,list(mydata$biosex, mydata$ed_cmplt),min)
by(mydata$bmi,list(mydata$biosex, mydata$ed_cmplt),max)
which(mydata$bmi==max(mydata$bmi))
max(mydata$bmi)
which(mydata$bmi==min(mydata$bmi))
min(mydata$bmi)

mydata$bmizscores<-(mydata$bmi-mean(mydata$bmi))/sd(mydata$bmi))
which(mydata$bmizscores==max(mydata$bmizscores))
max(mydata$bmizscores)
which(mydata$bmizscores==min(mydata$bmizscores))
min(mydata$bmizscores)

mydata$byfif<-(mydata$bmizscores*15)
mydata$byfifandhund<-(mydata$byfif+100)
mean(mydata$byfifandhund)
sd(mydata$byfifandhund)
which(mydata$byfifandhund==max(mydata$byfifandhund))
max(mydata$byfifandhund)
which(mydata$byfifandhund==min(mydata$byfifandhund))
min(mydata$byfifandhund)







