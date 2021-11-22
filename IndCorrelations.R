##Code to plot Leader-Follower dynamics

##Written by AA on 17July2019

fname <- file.choose()
dat = read.csv(fname, header=TRUE)
range=unique(dat$Frame)

#dat=na.omit(dat)
dat_out=data.frame()
#cp=1  ##Approach frame


##Clean the data (remove trajectories which are present less han 10% of time)
#dat=subset(dat, Frame > range[1200])
cut_l=150 #length(unique(dat$Frame))*0.2
dd=as.data.frame(table(dat$ID))
trails=dd$Var1[dd$Freq>round(cut_l)]
dat=subset(dat,ID %in% trails)

#Remove duplicate IDs

dat1=dat[!duplicated(cbind(dat$Frame,dat$ID)),]
dat=NA
dat=dat1


#range = unique(dat$Frame)
dat$x=(dat$xmin+dat$xmax)/2
dat$y=(dat$ymin+dat$ymax)/2

#dat=subset(dat,dat$Frame>15750)
#Variables

vel <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Frame","ID","v","head")
colnames(vel) <- x
medSpI=vector()

for(i in 1:(length(range)-10)){
  
    i_dat = dat[dat$Frame==range[i],]  
    i_dat1=dat[dat$Frame==range[i+10],]   #changes to smoothen detection error, velocity vector over 10 frames, 29/06/2020
    new_dat=merge(x=i_dat1,y=i_dat,by="ID")
    new_dat$vx=new_dat$x.y-new_dat$x.x
    new_dat$vy=new_dat$y.y-new_dat$y.x
    v = sqrt((new_dat$vx^2)+(new_dat$vy^2))
    fr = rep(range[i],nrow(new_dat))
    id=new_dat$ID
    heading= atan2(new_dat$vy,new_dat$vx)*180/pi
    vel=rbind(vel,cbind(Frame=fr,ID=id,v=v,head=heading))
    medSpI[i]=median(v)

}
head(vel)


## choose esc period to look at the initiation
vel1=vel[vel$Frame %in% range[1200:1800],]
fts = unique(vel1$ID)

library(TTR)
#smoothen the individual velocities for change point analysis
for(i in 1:length(fts)){
  dt_temp = vel1[vel1$ID==fts[i],]
  if(nrow(dt_temp)<31){
    next
  }
  dt_temp$v[is.na(dt_temp$v)]=0
  vel1$vs[vel1$ID==fts[i]] = SMA(dt_temp$v,n=30) ##smoothening to take care of fluctuations

}


#Change point analysis for the speed of individuals and heading
library(changepoint)
cpts=data.frame()



for(i in 1:length(fts)){
  
dt_temp = vel1[vel1$ID==fts[i],] 
#dt_temp = na.omit(dt_temp)
#if((nrow(dt_temp)<150) | (max(dt_temp$vs)<mean(na.omit(vel$vs)))){
#  next
#}     #commented on 24/06/2020, want to include all the individuals, however small there response may be

if((nrow(dt_temp)<90)){     ##at least 3 sec of data
  next
}
##For speed
vec=dt_temp$vs
vec[is.na(vec)]=0
mvalue=cpt.mean(vec, method="BinSeg",Q=5,penalty="None")
plot(mvalue,ylab="Speed",xlab="Frame number",xaxt="n",main=paste(fts[i]),ylim=c(0,max(na.omit(vel1$vs)))) 

cpts[i,"id"]=fts[i]
cpts[i,"cp1"]=dt_temp$Frame[mvalue@cpts[which((dt_temp$vs[(mvalue@cpts+5)]-dt_temp$vs[(mvalue@cpts-5)])>0)[1]]]  ##making sure that change is positive jump



}


##Code to see which timestamps are less than median change-point
## we are trying to identify the individuals whi increase their speed before the group response
## this means bfore the group median speed changes significantly* we identify this using change poin in median speed of the group)
hist(cpts$cp1)
median(hist(cpts$cp1))
cpts$id[which(cpts$cp1<12242)]   ## Type this value based on change-point analysis fronm the previou code


#############################

inirank=cpts[order(cpts$cp1),]   ##or select from here upto value calculated in the previous step
inirank$id[inirank$cp1<12242]
#Hierarchy of change-points
write.csv(file="Graphs/28MarchEve_01_02/response_Initiation.csv", x=cpts$id[order(cpts$cp1)])




#################################################################################
############################3For particlar events###########################
################################################################################
#pairwise cross-correlations for escape events,  set these in the beginning
st=range[6000]
sp=range[7500]
vel1<-NA
fts<-NA
vel1=vel[vel$Frame %in% (st:sp),]
fts = unique(vel1$ID)
#lead<- data.frame(nrows=length(fts)^2,ncol=2)
lead=vector()
lag=vector()
lagv=numeric()
corrs=numeric()
ct=1
for(i in 1:length(fts)){
  for(j in 1:length(fts)){
    if(i<j){
      dt_1 = vel1[vel1$ID==fts[i],]
      dt_2 = vel1[vel1$ID==fts[j],]
      if(nrow(dt_1)==0 | nrow(dt_2) == 0){
        next
      }
      
      ##merge two data drames according to frame number
      dt=merge(x=dt_1,y=dt_2,by="Frame")
      if(nrow(dt)==0){
        next
      }
      ##cross correlation for 2 individuals
      x=ccf(dt$v.x,dt$v.y,na.action = na.pass,lag.max=150,plot=FALSE)   ##max lag is 5 sec, 150 points out of 1000/1500
      l=x$lag[which(abs(x$acf)==max(abs(x$acf)))]
      if(length(l)==0){
        next
      }
      if(l<0){
        corrs[ct]=max(abs(x$acf)) 
        lead[ct]=as.character(fts[i])
        lag[ct]=as.character(fts[j])
        lagv[ct]=abs(l)
        ct=ct+1
      }else if(l>0){
        corrs[ct]=max(abs(x$acf))
        lead[ct]=as.character(fts[j])
        lag[ct]=as.character(fts[i])
        lagv[ct]=abs(l)
        ct=ct+1
        #}else{
        # lead[i,j]=-1
        
      }
    }
  }
}



##Draw the network
d=data.frame(lead,lag,lagv,corrs)

##Network analysis


##Put threshold on the correlation strength, high threshold for now = 0.5
d=subset(d,corrs>0.5)
d1=d[ , !(names(d) %in% "corrs")]
library('igraph')
net <- graph_from_data_frame(d=d1, vertices=fts, directed=T) 
E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
#V(net)$size <- log(closeness(net,mode="out")*100)*30
# node we'll create a new color variable as a node property
V(net)$color[closeness(net,mode="out") %in% sort(closeness(net,mode="out"),decreasing=TRUE)[1:5]] <- "green"
V(net)$color[closeness(net,mode="out") %in% sort(closeness(net,mode="out"),decreasing=FALSE)[1:5]] <- "red"

E(net)$type  # Edge attribute "type"
#V(net)$label <- ifelse( closeness(net,mode="out")==max(closeness(net,mode="out")), V(net)$name, NA )
#graphCol = pal(fine)[as.numeric(cut(closeness(net,mode="out"),breaks = fine))]  
plot(net,edge.arrow.size=.1,layout=layout_with_fr,edge.label=d$lagv)
legend(x=1, y=.75, legend=c("Leader", "Influencers","Followers","Isolated"),pch=21, pt.bg=c("green","blue","red","white"), pt.cex=2, bty="n")


##Community structure
coms=components(net,mode=c("weak","strong"))
hist(coms$csize,xlab="Group size",col="cyan",main=paste("Network modularity, N=",length(fts)))
table(coms$csize)

sort(closeness(net,mode="out"),decreasing=TRUE)
write.csv(file="Graphs/28MarchEve_01_02/InfluenceRank.csv", x=sort(closeness(net,mode="out"),decreasing =TRUE))


##Leader-follower pairs

write.csv(file="Graphs/28MarchEve_01_02/Leader_follower_pairs.csv", x=d)


#########################################################################################
###Sliding window approach for the network analysis
ccNet=vector() #Average closeness centrality of the network
N=length(range)-1
slide=300
w_size=900
#num_iter=ceiling(N/slide)-w_size+slide+1  ##not able to calculate number of iterations
curr_frame=1
cn=0
while(curr_frame<N){
  vel1=vel[vel$Frame %in% range[curr_frame]:min(range[(curr_frame+w_size)],range[N],na.rm=TRUE),]
  if((curr_frame+w_size)>N){
    break
  }
  curr_frame=curr_frame+slide
  cn=cn+1
  
  ##lead-lag analysis
  lead=vector()
  lag=vector()
  lagv=numeric()
  corrs=numeric()
  ct=1
  for(i in 1:length(fts)){
    for(j in 1:length(fts)){
      if(i<j){
        dt_1 = vel1[vel1$ID==fts[i],]
        dt_2 = vel1[vel1$ID==fts[j],]
        if(nrow(dt_1)==0 | nrow(dt_2) == 0){
          next
        }
        ##cross correlation for 2 individuals
        x=ccf(dt_1$v,dt_2$v,na.action = na.pass,lag.max=30,plot=FALSE)   ##set max lag to correlation length later
        l=x$lag[which(abs(x$acf)==max(abs(x$acf)))]
        if(l<0){
          #lead[i,j]=1 
          lead[ct]=as.character(fts[i])
          lag[ct]=as.character(fts[j])
          lagv[ct]=abs(l)
          corrs[ct]=max(abs(x$acf))
          ct=ct+1
        }else if(l>0){
          #lead[i,j]=0
          lead[ct]=as.character(fts[j])
          lag[ct]=as.character(fts[i])
          lagv[ct]=l
          corrs[ct]=max(abs(x$acf))
          ct=ct+1
          #}else{
          # lead[i,j]=-1
        }
        
      }
    }
  }
  
  ##Draw the network
  assign(paste("d",cn,sep="_"),data.frame(lead,lag,lagv,corrs))
  
  
}

library('igraph')
##Save the graphs and closeness centrality rank time-series
Inflnet<- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("window","ID","ccIndex")
colnames(Inflnet) <- x
for(i in 1:cn){
  
  ##Put threshold on the correlation strength, high threshold for now = 0.5
  d=subset(get(paste("d",i,sep="_")),corrs>0.5)
  net <- graph_from_data_frame(d=d, vertices=fts, directed=T) 
  ##Color the vertices for their closeness centrality
  fine = 1000 # this will adjust the resolving power.
  pal = colorRampPalette(c('white','red','blue','green'))
  
  #this gives you the colors you want for every point
  graphCol = pal(fine)[as.numeric(cut(closeness(net,mode="out"),breaks = fine))]  
  setwd("/media/akanksharathore/f41d5ac2-703c-4b56-a960-cd3a54f21cfb/aakanksha/Phd/Analysis/3_CollectiveEscape/Graphs/10MarchEve1_03_04/individual_networks/")
  png(filename=paste("d",i,".png",sep="_"))
  plot(net,edge.arrow.size=.1,layout=layout_with_fr,edge.label=d$lagv,vertex.color=graphCol)
  legend(x=1, y=.75, legend=c("Leader", "Influencers","Followers","Isolated"),pch=21, pt.bg=c("green","blue","red","white"), pt.cex=2, bty="n")
  dev.off();
  #closeness centrality
  clCent = as.data.frame(closeness(net,mode="out"))
  sort(closeness(net,mode="out"),decreasing =TRUE)
  Inflnet=rbind(Inflnet,cbind(window=rep(paste("d",i,sep="_"),nrow(clCent)),ID=rownames(clCent),ccIndex=clCent$`closeness(net, mode = "out")`))
  
  # Community detection (by optimizing modularity over partitions):
 # ceb <- cluster_edge_betweenness(net)
  #dendPlot(ceb, mode="hclust")
  
  
}


##Plot influence of all the individuals
Inflnet$ccIndex=as.numeric(as.character(Inflnet$ccIndex))
fts = unique(Inflnet$ID)
#fts=c(5,12,27,28,30,34)#,24,25,26)
for(i in 1:length(fts)){
  dt_temp = Inflnet[Inflnet$ID==fts[i],]
  xx=(1:length(dt_temp$ccIndex))
  if(i==1){
    plot(xx,dt_temp$ccIndex,col=fts[i],type="b",xaxt="n",pch=i,xlab="Slide window",ylab="Centrality index",ylim=c(0,max(Inflnet$ccIndex)))
  }else{
    points(xx,dt_temp$ccIndex,type="b",col=fts[i],pch=i)
  }
}

mtext(text=unique(dt_temp$window),side=1,at=xx)
legend("topright",legend=fts,col=fts,pch=(1:length(fts)))
abline(v=cp,col="black")

##Consistency of influence

meanInfl=tapply(as.numeric(as.character(Inflnet$ccIndex)),Inflnet$ID,mean)  ##Typical value
sdInfl=tapply(as.numeric(as.character(Inflnet$ccIndex)),Inflnet$ID,sd)    #consistency-lower the SD more consistent
sort(meanInfl,decreasing=TRUE)
sort(sdInfl)
