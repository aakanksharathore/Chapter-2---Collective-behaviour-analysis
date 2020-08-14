## leader-Follower analysis results\
##
library(ggplot2)
library(viridis)
##Part1 - community modularity - emergence of leader-follower dynamics
fname <- file.choose()   # Community structure.cs
dat = read.csv(fname, header=TRUE)
View(dat)


##
boxplot(X.clusters..2. ~ Event, data=dat,col="#009999",ylim=c(0,5),main="Number of clusters")
boxplot(X.solitary ~ Event, data=dat,col="#009999",main="No. of solitary")


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
