##Code to plot individual time-series, indovidual-group correlations and inter-individual correlations
##Written by AA on 17July2019

fname <- file.choose()
dat = read.csv(fname, header=TRUE)
dat=na.omit(dat)
dat_out=data.frame()
cp=1  ##Approach frame

o_range=unique(dat$Frame)
##Clean the data (remove trajectories which are present less han 10% of time)
#dat=subset(dat, Frame > range[1200])
cut_l=300 #length(unique(dat$Frame))*0.2
dd=as.data.frame(table(dat$ID))
trails=dd$Var1[dd$Freq>round(cut_l)]
dat=subset(dat,ID %in% trails)

#Remove duplicate IDs

dat1=dat[!duplicated(cbind(dat$Frame,dat$ID)),]
dat=NA
dat=dat1


range = unique(dat$Frame)
dat$x=(dat$xmin+dat$xmax)/2
dat$y=(dat$ymin+dat$ymax)/2

#dat=subset(dat,dat$Frame>15750)
#Variables

vel <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Frame","ID","v","head")
colnames(vel) <- x
medSpI=vector()

for(i in 1:length(range)){
  
  i_dat = dat[dat$Frame==range[i],]
  
  if(i>1){
    i_dat1=dat[dat$Frame==range[i-1],]
    new_dat=merge(x=i_dat1,y=i_dat,by="ID")
    new_dat$vx=new_dat$x.y-new_dat$x.x
    new_dat$vy=new_dat$y.y-new_dat$y.x
    v = (new_dat$vx^2)+(new_dat$vy^2)
    fr = rep(range[i],nrow(new_dat))
    id=new_dat$ID
    heading= atan2(new_dat$vy,new_dat$vx)*180/pi
    vel=rbind(vel,cbind(Frame=fr,ID=id,v=v,head=heading))
    medSpI[i]=median(v)
  }
  
  
}
head(vel)
fts = unique(vel$ID)
##Subset the data to remove the individuals who are present less tha 20%
# of the time after the threat event
# vel1=subset(vel,Frame>cp)
# cut_off=(1/5)*length(unique(vel1$Frame))
# dd=as.data.frame(table(vel1$ID))
# trails=dd$Var1[dd$Freq>cut_off]
# vel=subset(vel1,ID %in% trails)
library(TTR)
#smoothen the individual velocities for change point analysis
for(i in 1:length(fts)){
  dt_temp = vel[vel$ID==fts[i],]
  if(nrow(dt_temp)<31){
    next
  }
  vel$vs[vel$ID==fts[i]] = SMA(dt_temp$v,n=30) ##smoothening to one second
  
}


#Change point analysis for the speed of individuals and heading
library(changepoint)
cpts=data.frame()
cptl=data.frame()


for(i in 1:length(fts)){
  
dt_temp = vel[vel$ID==fts[i],] 
dt_temp = na.omit(dt_temp)
if((nrow(dt_temp)<150) | (max(dt_temp$vs)<mean(na.omit(vel$vs)))){
  next
}
##For speed
mvalue=cpt.mean(dt_temp$vs, method="BinSeg",Q=3,penalty="None")
plot(mvalue,ylab="Speed",xlab="Frame number",xaxt="n")
cpts[i,"id"]=fts[i]
cpts[i,"cp1"]=dt_temp$Frame[mvalue@cpts[which((dt_temp$vs[mvalue@cpts+30]-dt_temp$vs[(mvalue@cpts)])>0)[1]]]
cpts[i,"cp2"]=dt_temp$Frame[mvalue@cpts[which((dt_temp$vs[mvalue@cpts+30]-dt_temp$vs[(mvalue@cpts)])>0)[2]]]
cpts[i,"cpl"]=dt_temp$Frame[mvalue@cpts[length(mvalue@cpts)-1]]

##For heading
# mvalue=cpt.mean(dt_temp$head, method="BinSeg",Q=15,penalty="None")
# #plot(mvalue,ylab="Speed",xlab="Frame number",xaxt="n")
# cptl[i,"id"]=fts[i]
# cptl[i,"cp1"]=dt_temp$Frame[mvalue@cpts[1]]
# cptl[i,"cp2"]=dt_temp$Frame[mvalue@cpts[2]]
# cptl[i,"cpl"]=dt_temp$Frame[mvalue@cpts[length(mvalue@cpts)-1]]
}

##Changes needs to be done for cptl, not showing correct jumpes in heading
## heading doesn't seem like a relaible indicator of initiators as of now


#Hierarchy of change-points
write.csv(file="Graphs/17MarchEve_04_05/response_Initiation.csv", x=cpts$id[order(cpts$cp1)])
Init=cpts$id[order(cpts$cp1)][1]

cpts$id[order(cpts$cp1)]



# #lead<- data.frame(nrows=length(fts)^2,ncol=2)
# lead=vector()
# lag=vector()
# lagv=vector()
# corrs=vector()
# ct=1
# for(i in 1:length(fts)){
# for(j in 1:length(fts)){
#   if(i<j){
# dt_1 = vel1[vel1$ID==fts[i],]
# dt_2 = vel1[vel1$ID==fts[j],]
# if(nrow(dt_1)==0 | nrow(dt_2) == 0){
#  next
# }
# ##cross correlation for 2 individuals
# x=ccf(dt_1$v,dt_2$v,na.action = na.pass,lag.max=30,plot=FALSE)   ##set max lag to correlation length later
# l=x$lag[which(abs(x$acf)==max(abs(x$acf)))]
# corrs[ct]=max(abs(x$acf))
# if(l<0){
# #lead[i,j]=1 
#   lead[ct]=fts[i]
#   lag[ct]=fts[j]
#   lagv[ct]=abs(l)
#   ct=ct+1
# }else if(l>0){
#   #lead[i,j]=0
#   lead[ct]=fts[j]
#   lag[ct]=fts[i]
#   lagv[ct]=l
#   ct=ct+1
# #}else{
#  # lead[i,j]=-1
# }
#   
# }
# }
# }



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
  setwd("/media/akanksharathore/f41d5ac2-703c-4b56-a960-cd3a54f21cfb/aakanksha/Phd/Analysis/3_CollectiveEscape/Graphs/17MarchEve_04_05/individual_networks/")
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
#################################################################################
############################3For particlar events###########################
################################################################################
#pairwise cross
#pairwise cross-correlations for coordinated bouts,  set these in the beginning
st=range[2000]#27500#25350
sp=range[3500]#28000#26000
vel1=vel[which((vel$Frame>=st) & (vel$Frame<=sp)),]
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
      ##cross correlation for 2 individuals
      x=ccf(dt_1$v,dt_2$v,na.action = na.pass,lag.max=30,plot=FALSE)   ##set max lag to correlation length later
      l=x$lag[which(abs(x$acf)==max(abs(x$acf)))]
      
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

##Put threshold on the correlation strength, high threshold for now = 0.5
d=subset(d,corrs>0.5)
library('igraph')
net <- graph_from_data_frame(d=d, vertices=fts, directed=T) 
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

##Network analysis

#Degree centrality
centr_degree(net, mode = "out")

#closeness centrality
sort(closeness(net,mode="out"),decreasing=TRUE)
write.csv(file="Graphs/17MarchEve_04_05/Influence_event.csv", x=sort(closeness(net,mode="out"),decreasing =TRUE))


##Display on image
library(imager) 
im <- load.image(file.choose()) # open the jpeg image and read it into an object 'j'
plot(im)
points(dat$x[dat$ID==39],dat$y[dat$ID==39],col="red")
