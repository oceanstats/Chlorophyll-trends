#setup
library(nlme)
library(fields)
library(spTimer)
library(R.matlab)
library(sp)

setwd("~/Ch2")
rm(list=ls())

#data import
temp <- readMat("Longhurst_180.mat")#field of Longhurst locations
Longhurst <- temp$Longhurst
temp <- readMat("ESA_Data_360_180.mat")#chl data
lats <- temp$lats
lons <- temp$lons
chl <- temp$chl
temp <- readMat("SST_360_180.mat")#SST data
SST <- temp$SST
time <- 1:196

ind <- is.nan(chl)
chl[ind] <- NA #replacing Matlab's NaN with NA to fit rest of code

#temporary copies for restoring after each loop
Longhurststore <- Longhurst
lonsstore <- lons
latsstore <- lats
chlstore <- chl
SSTstore <- SST


area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean


for(j in area)
{
  print(j)
  #refer to backups as overwrite within j loop
  Longhurst <- Longhurststore
  lons <- lonsstore
    lats <- latsstore
    chl <- chlstore
    SST <- SSTstore

  
 if(exists("Model")==T){ rm("Model")} #rm last model for memory reasons
##Selecting required region (currently #6 high latitude N. Atlantic) 
#first replace not required with NA
boundind <- which(Longhurst!=j | is.na(Longhurst))
Longhurst[boundind] <- NA


for(i in 1:length(time)) 
{
  temp <- chl[,,i]
  temp[boundind] <- NA
  chl[,,i] <- temp
  tempS <- SST[,,i]
  tempS[boundind] <- NA
  SST[,,i] <- tempS
  

}

#then cut down to square of required (removing unnecessary area from equation)

boundind <- which(!is.na(Longhurst),arr.ind=T)
maxlatbound <- lats[max(boundind[,2])]
minlatbound <- lats[min(boundind[,2])]
maxlonbound <- lons[max(boundind[,1])]
minlonbound <- lons[min(boundind[,1])]




##sorting wrapping problems for spTmodel input (ie. in Pacific where jumps from -180 to 180 longitude)
longuse <- Longhurst # leave copy of full region for later use
swind <- 0
if(maxlonbound>100 & minlonbound< -50){
  ind <- lons< 0
  longuse<- rbind(longuse[ind==F,],longuse[ind==T,])
  
  for(i in 1:length(time)){chl[,,i]<- rbind(chl[ind==F,,i],chl[ind==T,,i])}
  for(i in 1:length(time)){SST[,,i]<- rbind(SST[ind==F,,i],SST[ind==T,,i])}

  lons <- c(lons[lons>=0],lons[lons<0]+360)
 
  boundind <- which(!is.na(longuse),arr.ind=T)
  maxlonbound <- lons[max(boundind[,1])]
  minlonbound <- lons[min(boundind[,1])]
  
  swind <- 1

} 


####################Spatio-temporal model

###load covariates and convert all required to vector form (for input into spTimer) (time first then longitude then latittude)
chl.dim <- dim(chl)

coordtemp <- expand.grid(time,lons,lats) #coordinates for every chl point in space and time 
chltemp <- (as.vector(aperm(chl,c(3,1,2))))
sitetemp <-rep(1:(chl.dim[1]*chl.dim[2]),each=196)  # site index
#TT=rep(((time-mean(time))/sd(time)), times=(chl.dim[1]*chl.dim[2])) #use normalised time
TT=rep(time, times=(chl.dim[1]*chl.dim[2])) 
SSTtemp <- as.vector(aperm(SST,c(3,1,2)))
monthtemp <-as.factor(rep(c(9:12,rep(1:12,times=16)),times=(chl.dim[1]*chl.dim[2])))

spTmodelFull <- data.frame(chl=chltemp,Longitude=coordtemp[[2]],Latitude=as.vector(coordtemp[[3]]),site=sitetemp,TT=TT,SST=SSTtemp,M=monthtemp)
spTmodel <- spTmodelFull


#remove sites where no data(i.e. NA for all time)
temp <- aggregate(chl~site,spTmodel,sum,na.action=na.pass,na.rm=T)
temp <- rep(temp[,2],each=196)
spTmodel$site[temp==0] <- NA
spTmodel <- spTmodel[!is.na(spTmodel$site),]
spTmodel$site <- rep(1:(nrow(spTmodel)/length(time)),each=196) #replace site list with valid sites


if(length(spTmodel)>(0.5*length(spTmodelFull))){          #ie. if more than 50% of data in region


rm(list=ls(pattern="temp")) #clear up of large temps
rm(list=c("spTmodelFull","chl","SST","TT"))
gc()


###knot setup
knotgrid <-spT.grid.coords(Longitude=c(max(spTmodel$Longitude-0.1),min(spTmodel$Longitude+0.1)),
                           Latitude=c(max(spTmodel$Latitude-0.1),min(spTmodel$Latitude+0.1)), 
                           by=c(round((max(spTmodel$Longitude)-min(spTmodel$Longitude))/4),
                                round((max(spTmodel$Latitude)-min(spTmodel$Latitude))/4)))#4 degree grid spacing


###selcting only those knots that are within  the required region.

#drawing polygon outline region
longuse[which(is.na(longuse))] <- 0 
cont <- contourLines(lons,lats,longuse,levels=1)

#are knots within region
temp2 <- 0
for(i in 1:length(cont)){
  temp <- cont[[i]]
  located <- point.in.polygon(knotgrid[,1],knotgrid[,2],temp$x,temp$y)
  located <- located + temp2
  temp2 <- located
}

ind <- located!=0
knotgrid <- knotgrid[ind,]




savename <- paste0(j,"_BGC_knotgrid.Rdata")
save(knotgrid, file=savename)


#time series setup
time.data <- spT.time(t.series=196,segments=1) 


####Modelrun
Model<- spT.Gibbs(
  formula = chl ~ TT+SST,
  data=spTmodel, model="GPP",
  coords=~Longitude+Latitude,
  knots.coords=knotgrid,
  distance.method="geodetic:km", 
  time.data=time.data, 
  report=100,
  nItr =40000, 
  nBurn=1000,  
  scale.transform="LOG",
  priors=spT.priors(model="GPP", inv.var.prior=Gamm(a=2,b=1),beta.prior=Norm(0,10^2), rho.prior=Norm(0,10^2)),
spatial.decay=spT.decay(distribution="FIXED",value=(3/1500)))



  
  
  #save used components of model (including what was previously saved in sptmodel input)
  betap <- Model$betap
  fitted <- Model$fitted
  model_input <- Model$data
   PMCC <- Model$PMCC
    rhop <- Model$rhop
  sig2eps <- Model$sig2eps
  sig2etap <- Model$sig2etap
  comp <- Model$computation.time
    temp <- spT.validation(Model$fitted[,1],Model$data$chl)
  RMSE <- as.numeric(temp[2])
    savename <- paste0(j,"_BGC_model_nm.Rdata")
save(betap,RMSE,fitted,model_input,PMCC,rhop,sig2eps,sig2etap,comp,file=savename)


}
}