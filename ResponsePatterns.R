## Code to analyse response patterns of blackbuck herds in predation events
#Written by AA on 24 June 2019
library(ggplot2)


#read data
fname <- file.choose()
dat = read.csv(fname, header=TRUE)
View(dat)

dat$SizeInt = cut(dat$Group.size, breaks= c(1,50,100,200,300,400),minmax=TRUE, oneval=TRUE)

##Plots

#Speed of response
ggplot(dat, aes(habitat, ..count..)) + geom_bar(aes(fill = speed), position = "dodge")
ggplot(dat, aes(beh.activity, ..count..)) + geom_bar(aes(fill = speed), position = "dodge")
ggplot(dat, aes(SizeInt, ..count..)) + geom_bar(aes(fill = speed), position = "dodge")


#Structure after response
ggplot(dat, aes(habitat, ..count..)) + geom_bar(aes(fill = strucrure), position = "dodge")
ggplot(dat, aes(beh.activity, ..count..)) + geom_bar(aes(fill = speed), position = "dodge")
ggplot(dat, aes(SizeInt, ..count..)) + geom_bar(aes(fill = speed), position = "dodge")