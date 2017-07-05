
library(spTimer)
library(R.matlab)

rm(list=ls())
setwd("~/Ch1-scripts")

##Setup for Longhurst and empty arrays
temp <- readMat("Longhurst_180.mat")
Longhurst <- temp$Longhurst
lats <- seq(-81.5,81.5,length.out=180);
lons <- seq(-179.5,179.5,length.out=360);

area <- sort(unique(Longhurst[!is.na(Longhurst)]))

  
ResultsTab <- rep(NA,times=54)
fittedChl<- array(NA,dim=c(360,180,196))
total_betap <- array(NA,dim=c(14,40000,length(area)))

##For each region combine all separate chains
for(j in area)
{
  
  print(j)
temp <- array(NA,dim=c(14,1))#for cbind to attach


if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54))#remove coastal,polar,uninished regions
{
 

}else{

  for(i in 1:40) #stitching together multiple runs
  {


     savename <- paste0("~/Ch1-scripts/chl_space_1500/",j,"_BGC_model_pt",i,".Rdata")
  load(savename)
  if(i==1)
    {
    temp2 <- array(NA,dim=c(nrow(fitted),40))#create on first iteration to guide rest
  } 
    temp <- cbind(temp,betap)
    temp2[,i] <- fitted[,1]
  }
  
  betap <- temp[,-1]#remove 1st column which is used for setup
  temp2 <- rowMeans(temp2,na.rm=T)
  fitted[,1] <- temp2
  

  #converting 1d fitted chl to 3d (lat, lon, mon)
fittedChl <- aperm(array(exp(fitted),dim=c(196,360,180)),c(2,3,1))

}
}
 
#plot traces
    savename <- paste0("~/Ch1-scripts/chl_space_1500/",j,"_trace.png")
    png(savename)
  plot(betap[2,],type='l',main=paste0("trace for region=",j),xlab="iteration",ylab="raw trend") 
dev.off()

total_betap[,,j] <- betap #combine each region into overall matrix 
}
}




save(total_betap,model_input,fittedChl,file="space_results.Rdata")
