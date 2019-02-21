getwd()
setwd("~/Desktop/204A")
mydata<-read.csv("204AHW1Data.csv")
dim(mydata)
sapply(mydata, class)
table(mydata$biosex)
table(mydata$ed_cmplt)
summary(mydata)
length(which(mydata[,2]>72))
mydata$bmi<-((mydata$weight_lbs/(mydata$height_in)^2)*703)
by(mydata$age_yr,mydata$ed_cmplt,mean)
aggregate(age_yr~biosex*ed_cmplt,data=mydata,mean)
by(mydata$bmi,mydata$biosex,mean)
"underweight"->mydata$bmiclass[mydata$bmi<18.5]
"normal"->mydata$bmiclass[mydata$bmi>=18.5&mydata$bmi<=24.9]
"overweight"->mydata$bmiclass[mydata$bmi>=25&mydata$bmi<=29.9]
"obese"->mydata$bmiclass[mydata$bmi>=30]
table(mydata$bmiclass)