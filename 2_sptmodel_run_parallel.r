
library(nlme,lib.loc="~/RPackages")

library(spTimer,lib.loc="~/RPackages")
library(R.matlab,lib.loc="~/RPackages")
library(sp,lib.loc="~/RPackages")
library(iterators,lib.loc="~/RPackages")
library(foreach,lib.loc="~/RPackages")
library(doParallel,lib.loc="~/RPackages")


setwd("~/RProject")
rm(list=ls())

#data
temp <- readMat("Longhurst_180.mat")
Longhurst <- temp$Longhurst

area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove polar,coastal, GoM, Archipelagic Deep basin, Mediterranean

part <- seq(1,41,1)
######create function for running burn in
dothisburn <- function(j){
savename <- paste0(j,"_BGC_input.Rdata")
load(savename)

####Simple model
time.data <- spT.time(t.series=196,segments=1) 

Model<- spT.Gibbs(
  formula = chl ~ TT+SST+M,
  data=spTmodel, model="GPP",
  coords=~Longitude+Latitude,knots.coords=knotgrid,
  distance.method="geodetic:km", time.data=time.data, report=1,
  nItr =1000, nBurn=0,  scale.transform="LOG",
  priors=spT.priors(model="GPP", inv.var.prior=Gamm(a=2,b=1),
  beta.prior=Norm(0,10^2), rho.prior=Norm(0,10^2)),
  spatial.decay=spT.decay(distribution="FIXED", value=(3/1500)))


  savename <- paste0(j,"_BGC_burn.Rdata")
  

  #save used components of model (including what was previously saved in sptmodel input)
  betap <- Model$betap
  fitted <- Model$fitted
  model_input <- Model$data
   PMCC <- Model$PMCC
    rhop <- Model$rhop
  sig2eps <- Model$sig2eps
  sig2etap <- Model$sig2etap
  comp <- Model$computation.time

  save(betap,fitted,model_input,PMCC,rhop,sig2eps,sig2etap,comp,init,file=savename)

   rm(list=c("Model","betap","fitted","model_input","PMCC","rhop","sig2eps","sig2etap"))
 
}
######create function for whole run
run_parallel <- function(i,j){
savename <- paste0(j,"_BGC_input.Rdata")
load(savename)
if(i>1)
{
  savename <- paste0(j,"_BGC_model_pt",i-1,".Rdata")#load last iteration
load(savename)

####Subsequent sets of model iterations
time.data <- spT.time(t.series=196,segments=1) 

Model<- spT.Gibbs(
  formula = chl ~ TT+SST+M,
  data=spTmodel, model="GPP",
  coords=~Longitude+Latitude,knots.coords=knotgrid,
  distance.method="geodetic:km", time.data=time.data, report=1,
  nItr =1000, nBurn=0,  scale.transform="LOG", initials=init,
  priors=spT.priors(model="GPP", inv.var.prior=Gamm(a=2,b=1),
  beta.prior=Norm(0,10^2), rho.prior=Norm(0,10^2)),
  spatial.decay=spT.decay(distribution="FIXED", value=(3/1500)))


  savename <- paste0(j,"_BGC_model_pt",i-1,".Rdata")
  

  #save used components of model (including what was previously saved in sptmodel input)
  betap <- Model$betap
  fitted <- Model$fitted
  model_input <- Model$data
   PMCC <- Model$PMCC
    rhop <- Model$rhop
  sig2eps <- Model$sig2eps
  sig2etap <- Model$sig2etap
  comp <- Model$computation.time
      leg <- length(rhop) 
  init=spT.initials(model="GPP",sig2eps=sig2eps[leg],sig2eta=sig2etap[leg],rho=rhop[leg],beta=betap[,leg])#initials from starting off next leg
  save(betap,fitted,model_input,PMCC,rhop,sig2eps,sig2etap,comp,file=savename)

   rm(list=c("Model","betap","fitted","model_input","PMCC","rhop","sig2eps","sig2etap"))
 } else{
 ####First 1000 model iterations
time.data <- spT.time(t.series=196,segments=1) 

Model<- spT.Gibbs(
  formula = chl ~ TT+SST+M,
  data=spTmodel, model="GPP",
  coords=~Longitude+Latitude,knots.coords=knotgrid,
  distance.method="geodetic:km", time.data=time.data, report=1,
  nItr =1000, nBurn=0,  scale.transform="LOG", 
  priors=spT.priors(model="GPP", inv.var.prior=Gamm(a=2,b=1),
  beta.prior=Norm(0,10^2), rho.prior=Norm(0,10^2)),
  spatial.decay=spT.decay(distribution="FIXED", value=(3/1500)))


  savename <- paste0(j,"_BGC_model_pt",(i-1),".Rdata")
  

  #save used components of model (including what was previously saved in sptmodel input)
  betap <- Model$betap
  fitted <- Model$fitted
  model_input <- Model$data
   PMCC <- Model$PMCC
    rhop <- Model$rhop
  sig2eps <- Model$sig2eps
  sig2etap <- Model$sig2etap
  comp <- Model$computation.time
      leg <- length(rhop) 
  init=spT.initials(model="GPP",sig2eps=sig2eps[leg],sig2eta=sig2etap[leg],rho=rhop[leg],beta=betap[,leg])#initials from starting off next leg
  save(betap,fitted,model_input,PMCC,rhop,sig2eps,sig2etap,comp,file=savename)

   rm(list=c("Model","betap","fitted","model_input","PMCC","rhop","sig2eps","sig2etap"))
 }
}
 #multi core setup
 
 registerDoParallel(cores=8)

 #run subsequent iterations
 foreach(i=part,.packages="spTimer")  %:% 
foreach(j=area,.packages="spTimer")  %dopar% 

{
  run_parallel(i,j)
}
