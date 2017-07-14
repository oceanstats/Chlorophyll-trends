library(spTimer)
library(R.matlab)

rm(list=ls())
setwd("~/Ch1-scripts")


temp <- readMat("Longhurst_180.mat")
Longhurst <- temp$Longhurst
lats <- seq(-81.5,81.5,length.out=180);
lons <- seq(-179.5,179.5,length.out=360);


chl <- temp$chl

area <- sort(unique(Longhurst[!is.na(Longhurst)]))

Longstore <- Longhurst  
ResultsTab <- rep(NA,times=54)
newc <- rep(NA,times=54)
fittedChl<- array(NA,dim=c(360,180,196))
fittedChl_sd<- array(NA,dim=c(360,180,196))
adjTrends <- array(NA,dim=c(360,180))
unadjTrends <- array(NA,dim=c(360,180))
total_betap <- array(NA,dim=c(14,2000,length(area)))
temp <- readMat("SST_360_180.mat")
SST <- temp$SST

overmodel <- 0
overobs <- 0
months <- 0

for(j in area)
{
  print(j)

Longhurst <- Longstore
#Longhurst area
  boundind <- which(Longhurst==j,arr.ind=T)

  
if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54))#remove coastal,polar,uninished regions
{
 

}else{
  
  savename <- paste0("~/Ch1-scripts/No_space/",j,"_BGC_model.Rdata")
  load(savename)
  #converting 1d fitted chl to 3d (lat, lon, mon)
  time <- 1:196

lonind <- which(model_input$Longitude>179.5)
model_input$Longitude[lonind] <- model_input$Longitude[lonind]-360# removing original correction for having longitudes increasing continuously

  #locations same for all time steps--> work out locations for plugging into fittedChl
  loc <- model_input[seq(1,length(model_input$Longitude),196),2:3] 
  locind <- array(NA,dim=c(nrow(loc),2))
  for(m in 1:nrow(loc))
{
  locind[m,1] <- which(lons==loc[m,1])
  locind[m,2] <- which(lats==loc[m,2])
}
  for(k in 1:196)
{
fittemp <- exp(fitted[seq(1+(k-1),length(model_input$Longitude),196),1])
for(m in 1:length(fittemp))
{
fittedChl[locind[m,1],locind[m,2],k] <- fittemp[m]
}
  }

total_betap[,,j] <- betap
}}



save(total_betap,model_input,fittedChl,file="no_space_results.Rdata")
