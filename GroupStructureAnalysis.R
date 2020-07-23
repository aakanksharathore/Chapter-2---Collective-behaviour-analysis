
fname <- file.choose()
GrpInt = read.csv(fname, header=TRUE)

##Group structure correlations

GrpInt$CCF.Pol.mSPI.sign = ifelse(GrpInt$CCF.Pol.mSPI>0,"pos",ifelse(GrpInt$CCF.Pol.mSPI==0,"0","neg"))
GrpInt$CCF.mnnd.pol.sign = ifelse(GrpInt$CCF.mnnd.pol>0,"pos",ifelse(GrpInt$CCF.mnnd.pol==0,"0","neg"))
GrpInt$CCF.mnnd.mSPI.sign = ifelse(GrpInt$CCF.mnnd.mSPI>0,"pos",ifelse(GrpInt$CCF.mnnd.mSPI==0,"0","neg"))

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

par(mfrow=c(3,1))
boxplot(abs(CCF.Pol.mSPI) ~ Event.type, data=GrpInt,col="lightblue",main="Pol~mSPI")
boxplot(abs(CCF.mnnd.pol) ~ Event.type, data=GrpInt,col="lightblue",main="mNND~Pol")
boxplot(abs(CCF.mnnd.mSPI) ~ Event.type, data=GrpInt,col="lightblue",main="mNND~mSPI")



##Variatiin in CCF-lag across videos for different events

par(mfrow=c(4,1))
x=(1:length(GrpInt$Video[GrpInt$Event.type=="Pre"]))

plot(GrpInt$CCF.mnnd.pol[GrpInt$Event.type=="Pre"]~x,pch=19,col="cyan",type="b",ylim=c(-1,1),ylab="CCF",xlab="Videos",main="Pre")
points(GrpInt$CCF.mnnd.mSPI[GrpInt$Event.type=="Pre"]~x,pch=23,col="orange",type="b")

plot(GrpInt$CCF.mnnd.pol[GrpInt$Event.type=="Esc"]~x,pch=19,col="cyan",type="b",ylim=c(-1,1),ylab="CCF",xlab="Videos",main="Esc")
points(GrpInt$CCF.mnnd.mSPI[GrpInt$Event.type=="Esc"]~x,pch=23,col="orange",type="b")

plot(GrpInt$CCF.mnnd.pol[GrpInt$Event.type=="Coord"]~x,pch=19,col="cyan",type="b",ylim=c(-1,1),ylab="CCF",xlab="Videos",main="Coord")
points(GrpInt$CCF.mnnd.mSPI[GrpInt$Event.type=="Coord"]~x,pch=23,col="orange",type="b")

plot(GrpInt$CCF.mnnd.pol[GrpInt$Event.type=="Post"]~x,pch=19,col="cyan",type="b",ylim=c(-1,1),ylab="CCF",xlab="Videos",main="Post")
points(GrpInt$CCF.mnnd.mSPI[GrpInt$Event.type=="Post"]~x,pch=23,col="orange",type="b")


##Position of randomly selected individuals

fname <- file.choose()   ##IndPosition.csv
PosDat = read.csv(fname, header=TRUE)


##
library(ggplot2)
ggplot(PosDat, aes( y=as.factor(Pos..mid.edge..pre), fill=Sex)) + 
  geom_bar(position="dodge", stat="count") + ggtitle("Before the perturbation") +ylab("Position")

ggplot(PosDat, aes( y=as.factor(na.omit(Pos..mid.edge..post)), fill=Sex)) + 
  geom_bar(position="dodge", stat="count") + ggtitle("After the perturbation") +ylab("Position")

## Front-back position during coordinated walk
ggplot(PosDat, aes( y=as.factor(na.omit(Pos..front.back..post)), fill=Sex)) + 
  geom_bar(position="dodge", stat="count") + ggtitle("After the perturbation") +ylab("Position")



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
  geom_bar(position="dodge", stat="count") + ggtitle("Consistency of positions") +ylab("Position")


## Pairs
fname <- file.choose()   ##Pairs.csv
pairD = read.csv(fname, header=TRUE)

ggplot(pairD, aes( y=Pair)) + 
  geom_bar(position="dodge", stat="count") +ylab("Pairs")








