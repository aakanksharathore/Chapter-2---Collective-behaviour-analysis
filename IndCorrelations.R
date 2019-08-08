##Code to plot individual time-series, indovidual-group correlations and inter-individual correlations
##Written by AA on 17July2019

fname <- file.choose()
dat = read.csv(fname, header=TRUE)
#View(dat)
dat_out=data.frame()
cp=4500

range = unique(dat$Frame)
dat$x=(dat$xmin+dat$xmax)/2
dat$y=(dat$ymin+dat$ymax)/2

#Variables

vel <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Frame","ID","v")
colnames(vel) <- x
medSpI=vector()
for(i in 1:length(range)){
  
  i_dat = dat[dat$Frame==range[i],]
  
  if(i>1){
    i_dat1=dat[dat$Frame==range[i-1],]
    new_dat=merge(x=i_dat1,y=i_dat,by="ID")
    new_dat$vx=new_dat$x.y-new_dat$x.x
    new_dat$vy=new_dat$y.y-new_dat$y.x
    new_dat$vy=sqrt((new_dat$vx^2)+(new_dat$vy^2))
    v = (new_dat$vx^2)+(new_dat$vy^2)
    fr = rep(range[i],nrow(new_dat))
    id=new_dat$ID
    vel=rbind(vel,cbind(Frame=fr,ID=id,v=v))
    medSpI[i]=median(v)
  }
  
  
}
View(vel)


#Plot time-series for the individual velocities
fts = unique(vel$ID)
fts=c(5,12,27,28,30,34)
for(i in 1:length(fts)){
  dt_temp = vel[vel$ID==fts[i],]
  if(i==1){
    plot(dt_temp$v~dt_temp$Frame,xlim=c(min(range),max(range)),ylim=c(min(vel$v),200),col=fts[i],type="b",xaxt="n")
  }else{
    points(dt_temp$v~dt_temp$Frame,type="b",col=fts[i])
  }
}
loc=seq(min(range),max(range),by=1000)
mtext(text=loc,side=1,at=loc)
legend("topright",legend=fts,col=fts,pch=20)
abline(v=range[cp],col="black")

#Change point analysis
library(changepoint)
cpts=data.frame()

for(i in 1:length(fts)){
  
dt_temp = vel[vel$ID==fts[i],] 
mvalue=cpt.mean(dt_temp$v, method="BinSeg",Q=4,penalty="None")
#plot(mvalue,ylab="Speed",xlab="Frame number",xaxt="n")
cpts[i,"id"]=fts[i]
cpts[i,"cp1"]=mvalue@cpts[1]
cpts[i,"cp2"]=mvalue@cpts[2]
cpts[i,"cpl"]=mvalue@cpts[length(mvalue@cpts)]
}



##Add some code to remove the individuals who disappear before the response event
#and if they are present for less than 20% frames during the lead-lag event

cpts=subset(cpts, cpl>cp) #when last point of change is before the approach


#Hierarchy of change-points
cpts$id[order(cpts$cp1)]
Init=cpts$id[order(cpts$cp1)][1]


#cross-correlation with global properties
vel1=vel[which((vel$Frame>=range[cpts[cpts$id==Init,"cp1"]]) & (vel$Frame<=range[cpts[cpts$id==Init,"cpl"]])),]

#lead<- data.frame(nrows=length(fts)^2,ncol=2)
lead=vector()
lag=vector()
lagv=vector()
ct=1
for(i in 1:length(fts)){
for(j in 1:length(fts)){
  if(i<j){
dt_1 = vel1[vel1$ID==fts[i],]
dt_2 = vel1[vel1$ID==fts[j],]
if(nrow(dt_1)==0 | nrow(dt_2) == 0){
 next
}
##Ind speed vs Median speed of individuals
x=ccf(dt_1$v,dt_2$v,na.action = na.pass,lag.max=10)   ##set max lag to correlation length later
l=x$lag[which(abs(x$acf)==max(abs(x$acf)))]
if(l<0){
#lead[i,j]=1 
  lead[ct]=fts[i]
  lag[ct]=fts[j]
  lagv[ct]=abs(l)
  ct=ct+1
}else if(l>0){
  #lead[i,j]=0
  lead[ct]=fts[j]
  lag[ct]=fts[i]
  lagv[ct]=l
  ct=ct+1
#}else{
 # lead[i,j]=-1
}
  
}
}
}


##Draw the network
d=as.data.frame(cbind(lead,lag,lagv))
library('igraph')
net <- graph_from_data_frame(d=d, vertices=fts, directed=T) 
E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"
plot(net,edge.label=d$lagv)

##Network analysis

#Degree centrality
centr_degree(net, mode = "in")

#closeness centrality
closeness(net,mode="in")
