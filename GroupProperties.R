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
dat_out=data.frame()
annd=vector()
Avel=vector()
pol=vector()
AvSpI = vector()
AvND=vector()
GrA=vector()
mnnd=vector()  #median nearest neighbour distance (might be a better metric than annd in our case)
medSpI=vector()
elon=vector()
Tilt=vector()
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
##################################################################################
###Group stucture ########################################################
#remove frames which have less than 3 individuals
for(i in 1:(length(range)-1)){
i_dat = na.omit(dat[dat$Frame==range[i],])
if(nrow(i_dat)<3){
  next
} 

##Calculations for group level properties

  #median NND
  mnnd[i]=median(nndist(i_dat$x,i_dat$y))

  #group elongation
  pc<-prcomp(i_dat[,c("x","y")], center = TRUE)
  pcs=summary(pc)
  pca1=max(pcs$importance[2,1],pcs$importance[2,2])
  pca2=min(pcs$importance[2,1],pcs$importance[2,2])
  elon[i]=1-pca2/pca1
  ##Slope
  pcSlope=pc$rotation["y","PC1"]/pc$rotation["x","PC1"]
  
 ##Average group velocity
    
    i_dat1=na.omit(dat[dat$Frame==range[i+1],])
    Avel[i]=sqrt( ((mean(i_dat1$x)-mean(i_dat$x))^2) + ((mean(i_dat1$y)-mean(i_dat$y))^2) )
    #Tilt
    AvelS = (mean(i_dat1$y)-mean(i_dat$y))/(mean(i_dat1$x)-mean(i_dat$x)) #Slope of group velocity
    Tilt[i] =atan(AvelS)*180/pi - atan(pcSlope)*180/pi
    
    new_dat=merge(x=i_dat1,y=i_dat,by="ID")
    new_dat$vx=new_dat$x.y-new_dat$x.x
    new_dat$vy=new_dat$y.y-new_dat$y.x
    new_dat$vm=sqrt((new_dat$vx^2)+(new_dat$vy^2))

    ## group polarisation
    new_dat1 = new_dat[new_dat$vm != 0, ]
    if((nrow(new_dat1) >= 3) & (nrow(new_dat)>3)){
      #Average individual speed
      AvSpI[i] = mean(new_dat$vm)
      medSpI[i]=median(new_dat$vm)
    new_dat$vx=new_dat$vx/new_dat$vm
    new_dat$vy=new_dat$vy/new_dat$vm
    vmod=sqrt(((sum(new_dat$vx))^2)+((sum(new_dat$vy))^2))
    pol[i]=vmod/nrow(new_dat)
    }else{
      next
    }

  }
  

##################Autocorrelation lengths#####################################
mnnd=na.omit(mnnd)
pol=na.omit(pol)
medSpI=na.omit(medSpI)
x=acf(mnnd,lag=3000,na.action=na.pass)
x=acf(medSpI,lag=1800,na.action=na.pass)
x=acf(pol,lag=1800,na.action=na.pass)
which(round(x$acf,digits=1)==0.0)[1]

 ##Plots
###################Group properties#############################################
loc=seq(1,range[length(range)],by=1000)
mm=floor((loc/30)/60)
ss=round((loc/30)-mm*60)

# median NND time-series

mnnd1=SMA(mnnd,n=300)
mnnd1=na.omit(mnnd1)
mvalue=cpt.mean(mnnd1, method="BinSeg",Q=3,penalty="None")
plot(mvalue,ylab="median NND",xlab="Time")#,xaxt="n")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")


#Polarization time-series
pol1=SMA(pol,n=300)
pol1=na.omit(pol1)
mvalue=cpt.mean(pol1, method="BinSeg",Q=3,penalty="None")
plot(mvalue,ylab="Polarization",xlab="Time",ylim=c(0,1))#,xaxt="n")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")

#Median individual speed
medSpI1=SMA(medSpI,n=300)
medSpI1=na.omit(medSpI1)
mvalue=cpt.mean(medSpI1, method="BinSeg",Q=2,penalty="None")
plot(mvalue,ylab="medSpI",xlab="Time")#,xaxt="n")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")


#Elongation
elon=na.omit(elon)
mvalue=cpt.mean(elon, method="BinSeg",Q=2,penalty="None")
plot(mvalue,ylab="Elongation",xlab="Time")#,xaxt="n",type="b")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")

#Tilt
Tilt=na.omit(Tilt)
mvalue=cpt.mean(Tilt, method="BinSeg",Q=2,penalty="None")
plot(mvalue,ylab="Tilt",xlab="Time")#,xaxt="n",type="b")
mtext(text=paste(mm,":",ss),side=1,at=loc)
abline(v=which(range==cp),col="red")



##################Group structure correlations####################################

##pre perturbation
pre=1:900

x=ccf(pol[pre],medSpI[pre],na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))

x=ccf(mnnd[pre],pol[pre],na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))

x=ccf(mnnd[pre],medSpI[pre],na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))

##During perturbation
dur=900:3000

x=ccf(pol[dur],medSpI[dur],na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))

x=ccf(mnnd[dur],pol[dur],na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))

x=ccf(mnnd[dur],medSpI[dur],na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))


##post perturbation 1
pos=6500:9000

x=ccf(pol[pos],medSpI[pos],na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))

x=ccf(mnnd[pos],pol[pos],na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))

x=ccf(mnnd[pos],medSpI[pos],na.action = na.pass,lag.max=1000)
mtext(paste(x$lag[which(abs(x$acf)==max(abs(x$acf)))]))


