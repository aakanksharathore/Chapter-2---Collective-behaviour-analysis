##Code for filling missing values of tracks
##Written by AA on 4th oct 2019

fname <- file.choose()
dat = read.csv(fname, header=TRUE)

#output data
datO = as.data.frame(matrix(ncol=6,nrow=length(fts)*length(frames)))
x <- c("Frame","ID","xmin","ymin","xmax","ymax")
colnames(datO) <- x
##Remove spurious IDs
##Clean the data (remove trajectories which are present less han 10% of time)
cut_l=length(unique(dat$Frame))*0.2
dd=as.data.frame(table(dat$ID))
trails=dd$Var1[dd$Freq>round(cut_l)]
dat=subset(dat,ID %in% trails)

#Unique Individual IDs
fts = unique(dat$ID)
frames=unique(dat$Frame)
##Correct data for each ID
k=1
j=1
for(i in 1:length(fts)){
 
  dt_temp=dat[dat$ID==fts[i],]
  if(nrow(dat_t)>0){
  for(j in 1:length(frames)){
    datO$ID[k]=fts[i]
    datO$Frame[k]=frames[j]
    if(frames[j] %in% dt_temp$Frame){

      datO$xmin[k]=dt_temp$xmin[dt_temp$Frame==frames[j]]
      datO$ymin[k]=dt_temp$ymin[dt_temp$Frame==frames[j]]
      datO$xmax[k]=dt_temp$xmax[dt_temp$Frame==frames[j]]
      datO$ymax[k]=dt_temp$ymax[dt_temp$Frame==frames[j]]
    }else if(j>1){

    datO$xmin[k]=datO$xmin[k-1]
    datO$ymin[k]=datO$ymin[k-1]
    datO$xmax[k]=datO$xmax[k-1]
    datO$ymax[k]=datO$ymax[k-1]
    }
    k=k+1
}
  }
}

write.csv(datO,"Data/26MarchEve_04_05__Corr.csv")
