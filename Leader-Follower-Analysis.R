## leader-Follower analysis results\
##
library(ggplot2)
library(viridis)
##Part1 - community modularity - emergence of leader-follower dynamics
fname <- file.choose()   # Community structure.cs
dat = read.csv(fname, header=TRUE)
View(dat)


##
boxplot(solitaryP ~ Event, data=dat,col="#009999",ylim=c(0,1),main="Number of clusters")
boxplot(X.solitary ~ Event, data=dat,col="#009999",main="No. of solitary")

ggplot(dat, aes(x=factor(Event,levels=c("Pre","Esc","Coord","Post")), y=solitaryP)) + geom_jitter(color="cyan4",width=0.06) + xlab("Event type")+
  ylab("Proportion of individuals who are solitary")+stat_summary(aes(y = solitaryP,group=1), fun.y=median, colour="red", geom="point",group=1)+
  theme_classic() # Classic theme

ggplot(dat, aes(x=factor(Event,levels=c("Pre","Esc","Coord","Post")), y=ConnectedN)) + geom_jitter(color="cyan4",width=0.06) + xlab("Event type")+
  ylab("Connected individuals")+stat_summary(aes(y = ConnectedN,group=1), fun.y=median, colour="red", geom="point",group=1)+
  theme_classic() # Classic theme
##################################  Response sequence  #################

fname=file.choose()  #responsepatterns_leaders.csv
resp_pat=read.csv(fname,header=TRUE)

##Quick responders/ Initiators

resp = resp_pat[resp_pat$Type=="Response",]

barplot(table(resp$Sex),xlab="Sex",main="N=50",col=c("cyan","orange","lightgreen","lightpink"))   #iNITIATORS SEX
barplot(table(resp$Position.Front.back.),xlab="Position",main="N=50",col=c("cyan","orange","lightgreen"))   #iNITIATORS position
barplot(table(resp$Position.edge.inside.),xlab="Position",main="N=50",col=c("cyan","orange","lightgreen"))   #iNITIATORS position


## Position
dat1=resp[resp$Sex=="F",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main=paste("N=",nrow(dat1)),col=c("cyan","orange","lightgreen","lightpink")) 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main=paste("N=",nrow(dat1)),col=c("cyan","orange","lightgreen","lightpink"))  

dat1=resp[resp$Sex=="BM",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main=paste("N=",nrow(dat1)),col=c("cyan","orange","lightgreen","lightpink")) 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main=paste("N=",nrow(dat1)),col=c("cyan","orange","lightgreen","lightpink")) 

dat1=resp[resp$Sex=="Juv",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main="N=9") 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main="N=10") 

## Last to respond
resp=NA
resp = resp_pat[resp_pat$Type=="Last",]

barplot(table(resp$Sex),xlab="Sex",main="N=50",col=c("cyan","orange","lightgreen","lightpink"))   #iNITIATORS SEX
barplot(table(resp$Position.Front.back.),xlab="Position",main="N=50",col=c("cyan","orange","lightgreen"))   #iNITIATORS position
barplot(table(resp$Position.edge.inside.),xlab="Position",main="N=50",col=c("cyan","orange","lightgreen"))   #iNITIATORS position


## Position
dat1=resp[resp$Sex=="F",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main=paste("N=",nrow(dat1))) 

dat1=resp[resp$Sex=="BM",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main=paste("N=",nrow(dat1))) 

dat1=resp[resp$Sex=="Juv",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main=paste("N=",nrow(dat1))) 

################# Proportions wrt actual distribution
fname <- file.choose()   ##PositionProportions.csv
PosProp = read.csv(fname, header=TRUE)

PosProp$Pos=factor(PosProp$Pos,levels=c("Far","Near","Mid"))

barplot(tapply(PosProp$Number,PosProp$Pos,FUN = sum),col=c("cyan","orange","lightgreen"),ylim=c(0,200))


barplot(tapply(PosProp$propI,PosProp$Pos,FUN = mean),col=c("cyan","orange","lightgreen"),main="N=50",ylab="Proportion responding")
barplot(tapply(PosProp$propL,PosProp$Pos,FUN = mean),col=c("cyan","orange","lightgreen"),main="N=50",ylab="Proportion responding")

##patterns for each video

ggplot(PosProp,aes(x=Video,y=propI))+
  geom_bar(stat="identity",aes(fill=Pos))+
  ylab("Proportion of initiators")+
  scale_fill_manual(values=c("cyan4","orange","purple"))+
  theme_classic() # Classic theme

ggplot(PosProp,aes(x=Video,y=propL))+
  geom_bar(stat="identity",aes(fill=Pos))+
  ylab("Proportion of late responders")+
  scale_fill_manual(values=c("cyan4","orange","purple"))+
  theme_classic() # Classic theme

################# Proportions wrt actual distribution
fname <- file.choose()   ##AScomposition.csv
ASprop = read.csv(fname, header=TRUE)

#PosProp$Pos=factor(PosProp$Pos,levels=c("Far","Near","Mid"))

barplot(tapply(ASprop$Number,ASprop$Sex,FUN = sum),col=c("cyan","orange","lightgreen","lightpink"),ylim=c(0,110))

barplot(tapply(ASprop$IniP,ASprop$Sex,FUN = mean),col=c("cyan","orange","lightgreen","lightpink"),main="N=50",ylab="Proportion responding")
barplot(tapply(ASprop$LastP,ASprop$Sex,FUN = mean),col=c("cyan","orange","lightgreen","lightpink"),main="N=50",ylab="Proportion responding")

##patterns for each video

ggplot(ASprop,aes(x=Vid,y=IniP))+
  geom_bar(stat="identity",aes(fill=Sex))+
  ylab("Proportion of initiators")+
  scale_fill_manual(values=c("cyan4","orange","purple","lightgreen"))+
  theme_classic() # Classic theme

ggplot(ASprop,aes(x=Vid,y=LastP))+
  geom_bar(stat="identity",aes(fill=Sex))+
  ylab("Proportion of late responders")+
  scale_fill_manual(values=c("cyan4","orange","purple","lightgreen"))+
  theme_classic() # Classic theme



#################### Leaders

lead= resp_pat[resp_pat$Type=="Leaders",]

barplot(table(lead$Sex),xlab="Sex",main=paste("N=",nrow(lead)),col=c("cyan","orange","lightgreen","lightpink"))   #Leader's sex
barplot(table(lead$Position.Front.back.),xlab="Position",main=paste("N=",nrow(lead)),col=c("cyan","orange","lightgreen","lightpink"))   #Leader's sex
barplot(table(lead$Position.edge.inside.),xlab="Position",main=paste("N=",nrow(lead)),col=c("cyan","orange","lightgreen","lightpink"))   #Leader's sex


## Position
dat1=lead[lead$Sex=="F",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main=paste("N=",nrow(dat1))) 

dat1=lead[lead$Sex=="M",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main=paste("N=",nrow(dat1))) 


## Isolated individuals
lead=NULL
lead= resp_pat[resp_pat$Type=="Isolated",]

barplot(table(lead$Sex),xlab="Sex",main=paste("N=",nrow(lead)),col=c("cyan","orange","lightgreen","lightpink"))   #Leader's sex
barplot(table(lead$Position.Front.back.),xlab="Position",main=paste("N=",nrow(lead)),col=c("cyan","orange","lightgreen","lightpink"))   #Leader's sex
barplot(table(lead$Position.edge.inside.),xlab="Position",main=paste("N=",nrow(lead)),col=c("cyan","orange","lightgreen","lightpink"))   #Leader's sex

## Position
dat1=lead[lead$Sex=="M",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main=paste("N=",nrow(dat1))) 

dat1=lead[lead$Sex=="BM",]
barplot(table(dat1$Position.Front.back.),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$Position.edge.inside.),xlab="Mid-periphery position",main=paste("N=",nrow(dat1))) 


################################ Leader-Follower pairs ##############################

fname=file.choose()  #responsepatterns_leader-follower.csv
lf_pair=read.csv(fname,header=TRUE)

barplot(table(lf_pair$SexL),xlab="Sex",main=paste("N=",nrow(lead)))   #Leader's sex

##When leaders are BM

dat1=lf_pair[lf_pair$SexL=="BM",]
barplot(table(dat1$PosL),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$SexF),xlab="Sex of followers",main=paste("N=",nrow(dat1))) 


##When leaders are Subadults

dat1=lf_pair[lf_pair$SexL=="M",]
barplot(table(dat1$PosL),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$SexF),xlab="Sex of followers",main=paste("N=",nrow(dat1))) 

##When leaders are F

dat1=lf_pair[lf_pair$SexL=="F",]
barplot(table(dat1$PosL),xlab="Front-Back position",main=paste("N=",nrow(dat1))) 
barplot(table(dat1$SexF),xlab="Sex of followers",main=paste("N=",nrow(dat1))) 


##Leader-follower proportions

barplot(tapply(ASprop$LeaderP,ASprop$Sex,FUN = mean),col=c("cyan","orange","lightgreen","lightpink"),main="N=50",ylab="Influential individuals")
barplot(tapply(ASprop$IsolatedP,ASprop$Sex,FUN = mean),col=c("cyan","orange","lightgreen","lightpink"),main="N=50",ylab="Isolated individuals")

fname <- file.choose()   ##FrontBackPositionProportions.csv
PosProp = read.csv(fname, header=TRUE)

#PosProp$Pos=factor(PosProp$Pos,levels=c("Far","Near","Mid"))

barplot(tapply(PosProp$Number,PosProp$Pos,FUN = sum),col=c("cyan","orange","lightgreen"),ylim=c(0,200))


barplot(tapply(PosProp$LeadI,PosProp$Pos,FUN = mean),col=c("cyan","orange","lightgreen"),main="N=50",ylab="Leaders")
barplot(tapply(PosProp$IsoI,PosProp$Pos,FUN = mean),col=c("cyan","orange","lightgreen"),main="N=50",ylab="Isolated")

##patterns for each video

ggplot(PosProp,aes(x=Video,y=propI))+
  geom_bar(stat="identity",aes(fill=Pos))+
  ylab("Proportion of initiators")+
  scale_fill_manual(values=c("cyan4","orange","purple"))+
  theme_classic() # Classic theme

ggplot(PosProp,aes(x=Video,y=propL))+
  geom_bar(stat="identity",aes(fill=Pos))+
  ylab("Proportion of late responders")+
  scale_fill_manual(values=c("cyan4","orange","purple"))+
  theme_classic() # Classic theme
