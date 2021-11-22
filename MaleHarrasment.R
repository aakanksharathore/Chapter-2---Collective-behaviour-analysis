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
p1=1053
p2=1281
#plot time-series for pair-wise distances
attach(pairAsso)
dvec=as.data.frame(pairAsso[(id2==p2)&(id1==p1),c('dist','ts')])
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