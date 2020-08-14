library(ggplot2)
fname <- file.choose()   #Groupinteractionrules.csv
GrpInt = read.csv(fname, header=TRUE)

##Group structure correlations

GrpInt$CCF.Pol.mSPI.sign = ifelse(GrpInt$CCF.Pol.mSPI>0,"pos",ifelse(GrpInt$CCF.Pol.mSPI==0,"0","neg"))
GrpInt$CCF.mnnd.pol.sign = ifelse(GrpInt$CCF.mnnd.pol>0,"pos",ifelse(GrpInt$CCF.mnnd.pol==0,"0","neg"))
GrpInt$CCF.mnnd.mSPI.sign = ifelse(GrpInt$CCF.mnnd.mSPI>0,"pos",ifelse(GrpInt$CCF.mnnd.mSPI==0,"0","neg"))

GrpInt$lag.Pol.mSPI.abs = abs(GrpInt$lag.Pol.mSPI)
GrpInt$lag.mnnd.pol.abs = abs(GrpInt$lag.mnnd.pol)
GrpInt$lag.mnnd.mSPI.abs = abs(GrpInt$lag.mnnd.mSPI)

par(mfrow=c(4,3))

#View(GrpInt[GrpInt$Event.type=="Pre",])

barplot(table(GrpInt$CCF.Pol.mSPI.sign[GrpInt$Event.type=="Pre"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")
barplot(table(GrpInt$CCF.mnnd.pol.sign[GrpInt$Event.type=="Pre"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")
barplot(table(GrpInt$CCF.mnnd.mSPI.sign[GrpInt$Event.type=="Pre"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")

barplot(table(GrpInt$CCF.Pol.mSPI.sign[GrpInt$Event.type=="Esc"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")
barplot(table(GrpInt$CCF.mnnd.pol.sign[GrpInt$Event.type=="Esc"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")
barplot(table(GrpInt$CCF.mnnd.mSPI.sign[GrpInt$Event.type=="Esc"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")

barplot(table(GrpInt$CCF.Pol.mSPI.sign[GrpInt$Event.type=="Coord"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")
barplot(table(GrpInt$CCF.mnnd.pol.sign[GrpInt$Event.type=="Coord"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")
barplot(table(GrpInt$CCF.mnnd.mSPI.sign[GrpInt$Event.type=="Coord"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")

barplot(table(GrpInt$CCF.Pol.mSPI.sign[GrpInt$Event.type=="Post"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")
barplot(table(GrpInt$CCF.mnnd.pol.sign[GrpInt$Event.type=="Post"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")
barplot(table(GrpInt$CCF.mnnd.mSPI.sign[GrpInt$Event.type=="Post"]),ylim=c(0,10),width=0.1,xlim=c(0,1),ylab="Frequency")


##Correlation strength

# par(mfrow=c(3,1))
# boxplot(CCF.Pol.mSPI ~ Event.type, data=GrpInt,col="lightblue",main="Pol~mSPI")
# boxplot(abs(CCF.mnnd.pol) ~ Event.type, data=GrpInt,col="lightblue",main="mNND~Pol")
# boxplot(abs(CCF.mnnd.mSPI) ~ Event.type, data=GrpInt,col="lightblue",main="mNND~mSPI")

ggplot(GrpInt, aes(x=factor(Event.type,levels=c("Pre","Esc","Coord","Post")), y=CCF.Pol.mSPI)) + geom_jitter(color="cyan4",width=0.06) + xlab("Event type")+
  ylab("Polarization~mSpeed")+stat_summary(aes(y = CCF.Pol.mSPI,group=1), fun.y=median, colour="red", geom="point",group=1)+
  theme_classic() # Classic theme

ggplot(GrpInt, aes(x=factor(Event.type,levels=c("Pre","Esc","Coord","Post")), y=CCF.mnnd.pol)) + geom_jitter(color="cyan4",width=0.06) + xlab("Event type")+
  ylab("Nearest-neighbour distance~Polarization")+stat_summary(aes(y = CCF.mnnd.pol,group=1), fun.y=median, colour="red", geom="point",group=1)+
  theme_classic() # Classic theme

ggplot(GrpInt, aes(x=factor(Event.type,levels=c("Pre","Esc","Coord","Post")), y=CCF.mnnd.mSPI)) + geom_jitter(color="cyan4",width=0.06) + xlab("Event type")+
  ylab("Nearest-neighbour distance~mSpeed")+stat_summary(aes(y = CCF.mnnd.mSPI,group=1), fun.y=median, colour="red", geom="point",group=1)+
  theme_classic() # Classic theme
  

##Patterns of lag
##Variatiin in CCF-lag across videos for different events


ggplot(GrpInt, aes(x=factor(Event.type,levels=c("Pre","Esc","Coord","Post")), y=lag.Pol.mSPI)) + geom_jitter(color="cyan4",width=0.06) + xlab("Event type")+
  ylab("Polarization~mSpeed")+stat_summary(aes(y = lag.Pol.mSPI,group=1), fun.y=median, colour="red", geom="point",group=1)+
  theme_classic() # Classic theme

ggplot(GrpInt, aes(x=factor(Event.type,levels=c("Pre","Esc","Coord","Post")), y=lag.mnnd.pol)) + geom_jitter(color="cyan4",width=0.06) + xlab("Event type")+
  ylab("Nearest-neighbour distance~Polarization")+stat_summary(aes(y = lag.mnnd.pol,group=1), fun.y=median, colour="red", geom="point",group=1)+
  theme_classic() # Classic theme

ggplot(GrpInt, aes(x=factor(Event.type,levels=c("Pre","Esc","Coord","Post")), y=lag.mnnd.mSPI)) + geom_jitter(color="cyan4",width=0.06) + xlab("Event type")+
  ylab("Nearest-neighbour distance~mSpeed")+stat_summary(aes(y = lag.mnnd.mSPI,group=1), fun.y=median, colour="red", geom="point",group=1)+
  theme_classic() # Classic theme



par(mfrow=c(4,1))
x=(1:length(GrpInt$Video[GrpInt$Event.type=="Pre"]))

plot(GrpInt$lag.mnnd.pol[GrpInt$Event.type=="Pre"]~x,pch=19,col="cyan",type="b",ylim=c(-150,150),ylab="CCF",xlab="Videos",main="Pre")
points(GrpInt$lag.mnnd.mSPI[GrpInt$Event.type=="Pre"]~x,pch=23,col="orange",type="b")

plot(GrpInt$lag.mnnd.pol[GrpInt$Event.type=="Esc"]~x,pch=19,col="cyan",type="b",ylim=c(-150,150),ylab="CCF",xlab="Videos",main="Esc")
points(GrpInt$lag.mnnd.mSPI[GrpInt$Event.type=="Esc"]~x,pch=23,col="orange",type="b")

plot(GrpInt$lag.mnnd.pol[GrpInt$Event.type=="Coord"]~x,pch=19,col="cyan",type="b",ylim=c(-150,150),ylab="CCF",xlab="Videos",main="Coord")
points(GrpInt$lag.mnnd.mSPI[GrpInt$Event.type=="Coord"]~x,pch=23,col="orange",type="b")

plot(GrpInt$lag.mnnd.pol[GrpInt$Event.type=="Post"]~x,pch=19,col="cyan",type="b",ylim=c(-150,150),ylab="CCF",xlab="Videos",main="Post")
points(GrpInt$lag.mnnd.mSPI[GrpInt$Event.type=="Post"]~x,pch=23,col="orange",type="b")


##Position of randomly selected individuals

fname <- file.choose()   ##IndPositions.csv
PosDat = read.csv(fname, header=TRUE)


##
library(ggplot2)
library(viridis)

#PosDat$Pos..front.back..post = factor(PosDat$Pos..mid.edge..post, levels=c("","Front","Mid","Back"))


ggplot(PosDat, aes( y=as.factor(Pos..mid.edge..pre), fill=Sex)) + 
  geom_bar(position="dodge", stat="count") + ggtitle("Before the perturbation") +ylab("Position")+
  scale_fill_viridis(discrete = TRUE)

ggplot(PosDat, aes( y=na.omit(Pos..mid.edge..post), fill=Sex)) + 
  geom_bar(position="dodge", stat="count") + ggtitle("After the perturbation") +ylab("Position")+
  scale_fill_viridis(discrete = TRUE)

## Front-back position during coordinated walk
ggplot(PosDat, aes( y=as.factor(na.omit(Pos..front.back..post)), fill=Sex)) + 
  geom_bar(position="dodge", stat="count") + ggtitle("After the perturbation") +ylab("Position")+
  scale_fill_viridis(discrete = TRUE)

##consistency of position
PosDat$Pos..mid.edge..post = factor(PosDat$Pos..mid.edge..post, levels=c("Edge", "Mid"))


for(i in 1:nrow(PosDat)){
  
  if(is.na(PosDat$Pos..mid.edge..post[i])){
    next
  }
  if(PosDat$Pos..mid.edge..pre[i]==PosDat$Pos..mid.edge..post[i]){
    PosDat$cons[i] = "same"
    
  }else{
    PosDat$cons[i] = "changed"
  }
}

##plot

ggplot(PosDat, aes( y=cons, fill=Sex)) + 
  geom_bar(position="dodge", stat="count") + ggtitle("Consistency of positions") +ylab("Position")+
  scale_fill_viridis(discrete = TRUE)


## Pairs
fname <- file.choose()   ##Pairs.csv
pairD = read.csv(fname, header=TRUE)

ggplot(pairD, aes( y=Pair)) + 
  geom_bar(position="dodge", stat="count") +ylab("Pairs")

##NULL model for the pairs association

#Combinations for n elements
n=39
pairsm = matrix(nrow=1000,ncol=n)

for(i in 1:1000){
  
pairsm[i,]=sample(x=combn(x=1:n, m=2, paste, collapse="-"),size=n)

}

perc=numeric()
for(i in 2:1000){
perc[i-1]=length(intersect(x=pairsm[1,],y=pairsm[i,]))
}

perc=(perc/n)*100
mean(perc)
range(perc)

fname <- file.choose()   ##associations_null_model.csv
assoN = read.csv(fname, header=TRUE)

barplot(cbind(assoN$percent,assoN$random),beside=TRUE,col=rep(c("cyan","orange"),times=c(10,10)),xlab="Videos",ylab="Percent associations")
legend("topleft",legend=c("Associations observed","Associations expected just by chance"),col=c("cyan","orange"),fill=c("cyan","orange"))


        