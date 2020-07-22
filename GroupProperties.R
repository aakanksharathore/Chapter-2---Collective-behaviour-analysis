## Code to measure group level properties in tracked videos
## Vsiualise the time-series for these properties and then chnage point analysis
#Written by AA on 20 March 2019
library(sp)
library(adehabitatHR)
library(spatstat) 
library(zoo)
library(changepoint)
library(TTR)

fname <- file.choose()
dat = read.csv(fname, header=TRUE)
dat=na.omit(dat)
head(dat)

##User-input (input the frame number for approach frame)
cp=1

##Clean the data
cut_l=300 #(remove IDs which are there for less than 10 seconds)
dd=as.data.frame(table(dat$ID))
trails=dd$Var1[dd$Freq>round(cut_l)]
dat=subset(dat,ID %in% trails)

#Remove duplicate IDs

dat1=dat[!duplicated(cbind(dat$Frame,dat$ID)),]
dat=NA
dat=dat1


############Ananalyze a subset###################
range = unique(dat$Frame)
#dat=dat[dat$Frame>=range[1] & dat$Frame <= range[length(range)],]


dat$x=(dat$xmin+dat$xmax)/2
dat$y=(dat$ymin+dat$ymax)/2

###Output
dat_out=data.frame(matrix(ncol = 5, nrow = length(range)))
colnames(dat_out)=c("Frame","mnnd","medSpI","pol","elon")
annd=vector()
Avel=vector()
AvSpI = vector()
AvND=vector()
GrA=vector()
Tilt=vector()
##################################################################################
###Group stucture ########################################################

for(i in 1:(length(range)-1)){
i_dat = na.omit(dat[dat$Frame==range[i],])

#remove frames which have less than 3 individuals
if(nrow(i_dat)<3){
  next
} 

##Calculations for group level properties

  #median NND
  dat_out$Frame[i] = i_dat$Frame[1]
  dat_out$mnnd[i]=median(nndist(i_dat$x,i_dat$y))

  #group elongation
  pc<-prcomp(i_dat[,c("x","y")], center = TRUE)
  pcs=summary(pc)
  pca1=max(pcs$importance[2,1],pcs$importance[2,2])
  pca2=min(pcs$importance[2,1],pcs$importance[2,2])
  dat_out$elon[i]=1-pca2/pca1
  ##Slope
  pcSlope=pc$rotation["y","PC1"]/pc$rotation["x","PC1"]
  
 ##Average group velocity
    
    i_dat1=na.omit(dat[dat$Frame==range[i+10],])  #changed to i+10 to smoothen the error in detection, 29/06/2020
    if(nrow(i_dat1) == 0){
      next
      }
    Avel[i]=sqrt( ((mean(i_dat1$x)-mean(i_dat$x))^2) + ((mean(i_dat1$y)-mean(i_dat$y))^2) )
    #Tilt
    AvelS = (mean(i_dat1$y)-mean(i_dat$y))/(mean(i_dat1$x)-mean(i_dat$x)) #Slope of group velocity
    tilt =abs(atan(AvelS)*180/pi - atan(pcSlope)*180/pi)
    if(tilt<135 & tilt>45){
      Tilt[i] = 1
    }else if(tilt<45 & tilt>135){
    Tilt[i] = 0
    }
    
    new_dat=merge(x=i_dat1,y=i_dat,by="ID")
    new_dat$vx=new_dat$x.y-new_dat$x.x
    new_dat$vy=new_dat$y.y-new_dat$y.x
    new_dat$vm=sqrt((new_dat$vx^2)+(new_dat$vy^2))

    ## group polarisation
    new_dat1 = new_dat[new_dat$vm != 0, ]
    if((nrow(new_dat1) >= 3) & (nrow(new_dat)>3)){
      #Average individual speed
      AvSpI[i] = mean(new_dat$vm)
      dat_out$medSpI[i]=median(new_dat$vm)
    new_dat$vx=new_dat$vx/new_dat$vm
    new_dat$vy=new_dat$vy/new_dat$vm
    vmod=sqrt(((sum(new_dat$vx))^2)+((sum(new_dat$vy))^2))
    dat_out$pol[i]=vmod/nrow(new_dat)
    }else{
      next
    }

  }
  

##################Autocorrelation lengths#####################################
mnnd=na.omit(dat_out$mnnd)
pol=na.omit(dat_out$pol)
medSpI=na.omit(dat_out$medSpI)
x=acf(mnnd,lag=3000,na.action=na.pass)
x=acf(medSpI,lag=3000,na.action=na.pass)
x=acf(pol,lag=3000,na.action=na.pass)
which(round(x$acf,digits=1)==0.0)[1]

 ##Plots
###################Group properties#############################################
loc=seq(1,range[length(range)],by=1000)
mm=floor((loc/30)/60)
ss=round((loc/30)-mm*60)

# median NND time-series

mnnd1=SMA(mnnd,n=300)
mnnd1=na.omit(mnnd1)
mvalue=cpt.mean(mnnd1, method="BinSeg",Q=5,penalty="None")
plot(mvalue,ylab="median NND",xlab="Time")#,xaxt="n")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")
mvalue

#Polarization time-series
pol1=SMA(pol,n=300)
pol1=na.omit(pol1)
mvalue=cpt.mean(pol1, method="BinSeg",Q=3,penalty="None")
plot(mvalue,ylab="Polarization",xlab="Time",ylim=c(0,1))#,xaxt="n")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")
mvalue

#Median individual speed
medSpI1=SMA(medSpI,n=300)
medSpI1=na.omit(medSpI1)
mvalue=cpt.mean(medSpI1, method="BinSeg",Q=3,penalty="None")
plot(mvalue,ylab="medSpI",xlab="Time")#,xaxt="n")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")
mvalue

#Elongation
elon=na.omit(dat_out$elon)
elon1=SMA(elon,n=300)
elon1=na.omit(elon1)
mvalue=cpt.mean(elon1, method="BinSeg",Q=2,penalty="None")
plot(mvalue,ylab="Elongation",xlab="Time")#,xaxt="n",type="b")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")

#Tilt
Tilt=na.omit(Tilt)
mvalue=cpt.mean(Tilt, method="BinSeg",Q=2,penalty="None")
plot(Tilt,ylab="Tilt",xlab="Time")#,xaxt="n",type="b")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")



##################Group structure correlations####################################

ran=4500:6000      ##change this range to calculate for specific events

dt=dat_out[ran,]

x=ccf(dt$pol,dt$medSpI,na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))

x=ccf(dt$mnnd,dt$pol,na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))

x=ccf(dt$mnnd,dt$medSpI,na.action = na.pass,lag.max=150)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))


###############################################################################
### Choose 5 random individuals


fts = sample(x=unique(dat$ID[1:1000]),size=5)


############################################################################
####Pairs maintained before and after the event ##############################


###Calculate average distance between pairs in a window before the event
pair_mat = data.frame(matrix(ncol=4,nrow=50))
colnames(pair_mat)=c("p1","p2","po1","po2")
pre=1:1500
ind=unique(dat$ID[dat$Frame %in% range[pre]]) 
dt=dat[dat$Frame %in% range[pre],]
dist_pre = matrix(nrow=length(ind),ncol=length(ind),dimnames=list(ind,ind))
for(i in 1:length(ind)){
  for(j in 1:length(ind)){
    if(i!= j){
    dist_pre[i,j] = mean((dt$x[dt$ID==ind[i]]-dt$x[dt$ID==ind[j]])^2 + (dt$y[dt$ID==ind[i]]-dt$y[dt$ID==ind[j]])^2)
    }
    }
}

##Find closest pairs
for(i in 1:length(ind)){
  x=which(dist_pre[i,]==min(na.omit(dist_pre[i,])),arr.ind=TRUE)
  pair_mat$p1[i]=ind[i]
  pair_mat$p2[i]=rownames(dist_pre)[x]
  
} 


###Calculate average distance between pairs in a window after the event

pre=3500:5000
ind=unique(dat$ID[dat$Frame %in% range[pre]]) 
dt=dat[dat$Frame %in% range[pre],]
dist_pre = matrix(nrow=length(ind),ncol=length(ind),dimnames=list(ind,ind))
for(i in 1:length(ind)){
  for(j in 1:length(ind)){
    if(i!= j){
      dist_pre[i,j] = mean((dt$x[dt$ID==ind[i]]-dt$x[dt$ID==ind[j]])^2 + (dt$y[dt$ID==ind[i]]-dt$y[dt$ID==ind[j]])^2)
    }
  }
}

##Find closest pairs
for(i in 1:length(ind)){
  x=which(dist_pre[i,]==min(na.omit(dist_pre[i,])),arr.ind=TRUE)
  pair_mat$po1[i]=ind[i]
  pair_mat$po2[i]=rownames(dist_pre)[x]
  
} 

write.csv(file="/media/akanksharathore/f41d5ac2-703c-4b56-a960-cd3a54f21cfb/aakanksha/Phd/Analysis/3_CollectiveEscape/Graphs/26MarchEve_04_05/pairs.csv",x=pair_mat)
###############################################################################
############################Which individuals are outliers?##############


