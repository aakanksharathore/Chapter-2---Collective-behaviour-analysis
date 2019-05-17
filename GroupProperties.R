## Code to measure group level properties in tracked videos
## Vsiualise the time-series for these properties and then chnage point analysis
#Written by AA on 20 March 2019
library(sp)
library(adehabitatHR)
library(spatstat)  
fname <- file.choose()
dat = read.csv(fname, header=TRUE)
View(dat)
dat_out=data.frame()
annd=vector()
Avel=vector()
pol=vector()
AvSpI = vector()
AvND=vector()
GrA=vector()



############Ananalyze a subset###################
range=908:1238
dat=dat[dat$Frame>=range[1] & dat$Frame <= range[length(range)],]
num_iter = length(unique(dat$Frame))


dat$x=(dat$xmin+dat$xmax)/2
dat$y=(dat$ymin+dat$ymax)/2
##################################################################################
###Group stucture ########################################################
for(i in 1:num_iter){
i_dat = dat[dat$Frame==i-1,]
 

##Calculations for group level properties
  ##ANND
  annd[i]=mean(nndist(i_dat$x,i_dat$y))
  #Average distance between individuals
#   ndMat = matrix(ncol=nrow(i_dat), nrow=nrow(i_dat))
#   sum=0
#   for (k in 1:nrow(i_dat)){
# 
#     for (j in 1:nrow(i_dat)){
#       ndMat[k,j] = sqrt(sum(( (i_dat$x[k] - i_dat$x[j])^2 ),( (i_dat$x[k] - i_dat$x[j])^2 )))
#       if(j>k)
#         sum=sum+ndMat[k,j]
#   }}
# AvND[i] = sum/nrow(i_dat)
  #group spread (normalised by group size)
xy <- data.frame('x'=i_dat$x,'y'=i_dat$y, 'id' = rep('a', nrow(i_dat)))
coordinates(xy) <- xy[, c('x', 'y')]
 GrA[i] = (mcp(xy[, 'id'], percent = 98)[[2]])/(nrow(i_dat))
 ##Average group velocity
  if(i<num_iter){
    
    i_dat1=dat[dat$Frame==(i+1),]
    Avel[i]=sqrt( ((mean(i_dat1$x)-mean(i_dat$x))^2) + ((mean(i_dat1$y)-mean(i_dat$y))^2) )
    ## group polarisation
    new_dat=merge(x=i_dat1,y=i_dat,by="Id")
    new_dat$vx=new_dat$x.y-new_dat$x.x
    new_dat$vy=new_dat$y.y-new_dat$y.x
    new_dat$vm=sqrt((new_dat$vx^2)+(new_dat$vy^2))
    new_dat$vx=new_dat$vx/new_dat$vm
    new_dat$vy=new_dat$vy/new_dat$vm
    vmod=sqrt(((sum(new_dat$vx))^2)+((sum(new_dat$vy))^2))
    pol[i]=vmod/nrow(new_dat)
    #Average individual speed
    AvSpI[i] = mean(new_dat$vm)
  }
  
}


##Plots

# ANND time-series
plot(annd,type="b")
#Average group velocity time-series
plot(Avel,type="b")
#Polarity time-series
plot(pol,type="b")
#Average individual speed
plot(AvSpI,type="b")
#Group spread time-series
plot(GrA,type="b")

####################################################################################################################################
##Check for signatures of harassers in their paiwise distance time-series
#########################################################################################

speeData = data.frame()
##calculate individual-speed time-series
for(i in range[-length(range)]){
  i_dat = dat[dat$Frame==i,]    #select data frame-wise
  i_dat1= dat[dat$Frame==i+1,] 
  df<-merge(x=i_dat,y=i_dat1,by="Id",all=FALSE)
  idd = df$Id
  dis = sqrt((df$x.x-df$x.y)^2 + (df$y.x - df$y.y)^2 )
  speeData <- rbind(speeData, data.frame(df$Frame.x,idd, dis))
  
}


pairAsso=data.frame()  #create an empty list t store the output

ts=vector()
id1=vector()
id2=vector()
dist=vector()
t=1
for(i in range[-length(range)]){
  i_dat = dat[dat$Frame==i,]    #select data frame-wise
  
  #Average distance between individuals
  #ndMat = matrix(ncol=nrow(i_dat), nrow=nrow(i_dat)) #matrix to store pair-wise distances
    
  for (k in 1:nrow(i_dat)){
      for (j in 1:nrow(i_dat)){
        dis = sqrt(sum(( (i_dat$x[k] - i_dat$x[j])^2 ),( (i_dat$x[k] - i_dat$x[j])^2 )))
        if(j>k){   #Take only upper-diagonal elements
          ts[t] = i
          id1[t] =  i_dat$Id[k]
          id2[t] = i_dat$Id[j]
          dist[t] = dis
          t=t+1
        }
        }}
  }

pairAsso=cbind(ts,id1,id2,dist)
head(pairAsso)
#write.csv(file="pairAsso.csv",x=pairAsso)

#select a pair
p1=1330
p2=1281
#plot time-series for pair-wise distances
attach(pairAsso)
dvec=as.data.frame(pairAsso[(id2==p1)&(id1==p2),c('dist','ts')])
head(dvec)

spee=as.data.frame(speeData[(speeData$idd==p1),c('dis','df.Frame.x')])
colnames(spee)[2]='ts'
spee1=as.data.frame(speeData[(speeData$idd==p2),c('dis','df.Frame.x')])
colnames(spee1)[2]='ts'
allData=merge(merge(dvec,spee,by="ts",all=FALSE),spee1,by="ts",all=FALSE)

plot(allData$dist~allData$ts,xlab="Frame number",ylab="Pair-wise distance",main=paste("Male ID - ",p1,", Female ID - ",p2),pch=8)
par(new=TRUE)
plot(allData$dis.x~allData$ts,col="orange",pch=17,axes=FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(allData$dis.x)))
mtext("speed", side=4, line=3)
points(allData$dis.y~allData$ts,col="blue",pch=10)
legend("topright",legend=c("distance","male speed","female speed"),col=c("black","orange","blue"),pch=c(8,17,10))


#calculate cross correlation between distance between pairs and ind speed
ccf(allData$dist,allData$dis.x,na.action = na.pass)
ccf(allData$dist,allData$dis.y,na.action = na.pass)
ccf(allData$dis.x,allData$dis.y,na.action = na.pass)
