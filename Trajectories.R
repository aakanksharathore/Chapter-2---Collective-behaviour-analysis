fname <- file.choose()
dat = read.csv(fname, header=TRUE)

##Clean the data (remove trajectories which are present less han 10% of time)
cut_l=length(unique(dat$Frame))*0.2
dd=as.data.frame(table(dat$ID))
trails=dd$Var1[dd$Freq>round(cut_l)]
dat=subset(dat,ID %in% trails)

#Plot time-series for the individual velocities
fts = unique(dat$ID)



for(i in 1:length(fts)){
  dt_temp = dat[dat$ID==fts[i],]
  if(i==1){
    plot(dt_temp$ymin~dt_temp$xmin,xlim=c(min(dat$xmin),max(dat$xmin)),ylim=c(min(dat$ymin),max(dat$ymin)),col=i,type="b")
  }else{
    points(dt_temp$ymin~dt_temp$xmin,type="b",col=i)
  }
}
