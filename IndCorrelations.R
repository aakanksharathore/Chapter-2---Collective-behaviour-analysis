##Code to plot Leader-Follower dynamics

##Written by AA on 17July2019

fname <- file.choose()
dat = read.csv(fname, header=TRUE)
range=unique(dat$Frame)

##Output path
pname = "Graphs/28MarchEve_01_02/"

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
vel1=vel[vel$Frame %in% range[1200:2250],]
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
plot(mvalue,ylab="Speed",xlab="Frame number",xaxt="n",main=paste(fts[i]),ylim=c(0,100)) 

cpts[i,"id"]=fts[i]
#cpts[i,"cp1"]=dt_temp$Frame[mvalue@cpts[which((dt_temp$vs[(mvalue@cpts+5)]-dt_temp$vs[(mvalue@cpts-5)])>10)[1]]]  ##How much change is necessary?? in terms of body lengths
param.est(mvalue)
#lst.cp = mvalue@cpts[which(diff(param.est(mvalue)$mean)>0)][-1]   ##identify the change-points woith positive jump and ignore first one
#thresh.cp = mvalue@cpts[which(param.est(mvalue)$mean>10)]    ## set min velocity equal to body length
pos.cp=mvalue@cpts[which((diff(param.est(mvalue)$mean)>0) & (param.est(mvalue)$mean>10))] ##position of positive changepoints with min speed of 10 i.e. one bodylength 29/11/2021
cpts[i,"cp1"]=dt_temp$Frame[pos.cp][1]

}
cpts[order(cpts$cp1),]
median(cpts$cp1)
##Code to see which timestamps are less than median change-point
## we are trying to identify the individuals whi increase their speed before the group response
## this means bfore the group median speed changes significantly* we identify this using change poin in median speed of the group)
#hist(cpts$cp1)
#median(hist(cpts$cp1))
#cpts$id[which(cpts$cp1<25043)]   ## Type this value based on change-point analysis fronm the previou code


#############################

inirank=cpts[order(cpts$cp1),]   ##or select from here upto value calculated in the previous step
##Take initiators as the individuals whose rank is less than 1/3rd quartile
top=round(length(fts)/3)
#inirank$id[inirank$cp1<25043]
#Hierarchy of change-points
write.csv(file=paste(pname,"response_Initiation.csv"), x=cpts$id[order(cpts$cp1)])




#################################################################################
############################3For particlar events###########################
################################################################################
#pairwise cross-correlations for escape events,  set these in the beginning
st=range[1200]
sp=range[2250]
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
write.csv(file=paste(pname,"network_matrix.csv"), x=d)


##Network analysis


##Put threshold on the correlation strength, high threshold for now = 0.5
d1=subset(d,corrs>0.5)
d1=d1[ , !(names(d) %in% "corrs")]
library('igraph')
net <- graph_from_data_frame(d=d1, vertices=fts, directed=T) 
net <- set_edge_attr(net, "weight", value= d1$lagv)              ##making it a weighted graph
E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"

plot(net,edge.size=3,layout=layout_with_fr,edge.label=d1$lagv)


##Community structure
coms=components(net,mode=c("weak","strong"))
hist(coms$csize,xlab="Group size",col="cyan",main=paste("Network modularity, N=",length(fts)))
table(coms$csize)

#### Code foe identifying the influential individuals 01/12/2021

## First calculate the number of total paths from each node, we will rank nodes in this order and look at the average path length for ties

##Output data frame
rankLeaders = data.frame(ID=as.vector(V(net)),npath=NA,dist=NA)

for(i in 1:length(V(net))){
  
  
spaths=distances(
  net,
  v = V(net)[i],
  to = V(net),
  mode = c("out"),
  weights = d1$lagv,
  algorithm = c("automatic", "unweighted", "dijkstra", "bellman-ford", "johnson")
)

rankLeaders$ID[i] = V(net)[i]
rankLeaders$npath[i] = length(which(!is.infinite(spaths)))-1
rankLeaders$dist[i] = mean(spaths[which(!is.infinite(spaths))])


}

## Sort on the basis of number of connections and mean length
rankLeaders[order(rankLeaders$npath,rankLeaders$dist,decreasing = c(TRUE,FALSE)),]
write.csv(file=paste(pname,"influence_rank.csv"), x=sort(closeness(net,mode="out"),decreasing =TRUE))


##Leader-follower pairs

write.csv(file=paste(pname,"LF_pairs.csv"), x=d)


#########################################################################################
