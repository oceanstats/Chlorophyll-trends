
library(spTimer)
library(fields)
library(R.matlab)

rm(list=ls())
setwd("~/Ch1-scripts")

#setup
temp <- read.csv(file="Longhurst_area.csv",header=F)#physical area (km2) of each longhurst region
Area_values <- c(as.matrix(temp))
temp <- readMat("Longhurst_180.mat")
Longhurst <- temp$Longhurst
temp <- readMat("ESA_Data_360_180.mat")
lats <- c(temp$lats)
lons <- c(temp$lons)


chl <- temp$chl

area <- sort(unique(Longhurst[!is.na(Longhurst)]))



temp <- readMat("SST_360_180.mat")
SST <- temp$SST

#load model runs for both models with and without spatial correaltion
setwd("~/Ch1-scripts/Comparison_1500")
load("space_results.Rdata")
space_betap <- total_betap

space_fittedChl <- fittedChl
Longhurst_space <- Longhurst


load("no_space_results.Rdata")
betap <- total_betap



rm("total_betap")
corc <- c(21,8,54,4,38,10,2,12,33,3,29,20,14,37,32,31,6,13,17,40,42,15,45,49,9,11,7,35,19,22,30,36,34,43,5,18,46,41,44,1,24,25,16,52,51,50,28,47,48,23,53,39,26,27)
area <- area[order(match(corc,area))]#sorting longhurst order to match plot available at http://www.marineplan.es/ES/fichas_kml/mapasfichas/reg_biogeog.jpg
indio <- 0
Area_values_index <- 0
ind_AV <- Area_values_index 
ResultsTab_space <- array(NA,dim=c(23,5))
ResultsTab <- array(NA,dim=c(23,5))
Chlcontent <- array(NA,dim=c(23))
regave <- array(NA,dim=c(196,54))
regave_space <- array(NA,dim=c(196,54))
regave_nospace <- array(NA,dim=c(196,54))
Test <- array(NA,dim=c(23))
trend_betap2 <- array(NA,dim=c(2000,23))
trend_betap2_space <- array(NA,dim=c(40000,23))
colnames(ResultsTab) <- c("Region","Trend Value","Lower Confidence Interval","Upper confidence interval","RMSE")
colnames(ResultsTab_space) <- c("Region","Trend Value","Lower Confidence Interval","Upper confidence interval","RMSE")

#remove from plotting unwanted regions
for(j in area){
if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54))#remove coastal,polar,uninished regions
{
  Longhurst_space[which(Longhurst_space==j)] <- NA
}
if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54))#remove coastal,polar,uninished regions
{
  Longhurst[which(Longhurst==j)] <- NA
}
}
#save files for restoration at each interval
chl_store <- chl
space_fittedChl_store <- space_fittedChl
fittedChl_store <- fittedChl

###for each region determine trends etc. and produce plots
for(j in area)
{
  ind_AV <- ind_AV+1 #index for correct placing of longhurst area values within array
  print(j)
chl <- chl_store
  space_fittedChl <- space_fittedChl_store
fittedChl <- fittedChl_store
  
if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54))#remove coastal,polar,uninished regions
{

} else{
  indio <- indio+1

##for each region  replace not required with NA
boundind <- which(Longhurst!=j | is.na(Longhurst))
for(i in 1:196) 
{
  temp <- chl[,,i]
  temp[boundind] <- NA
  chl[,,i] <- temp
  
  temp <- fittedChl[,,i]
  temp[boundind] <- NA
  fittedChl[,,i] <- temp
  
    temp <- space_fittedChl[,,i]
  temp[boundind] <- NA
  space_fittedChl[,,i] <- temp

}

##convert trend values to percent per year ( from decimal change per month)
space_betap[2,,j] <- 100*12*space_betap[2,,j]
betap[2,,j] <- 100*12*betap[2,,j]

##calculate 95% credibility intervals for trend
bounds <- array(NA,dim=c(14,2))
bounds_space <- bounds
for(n in 1:14)
{
  bounds[n,] <- c(mean(betap[n,,j])-sd(betap[n,,j])*2,mean(betap[n,,j])+sd(betap[n,,j])*2)#calculatiung confidence intervals
}

for(n in 1:14)
{
  bounds_space[n,] <- c(mean(space_betap[n,,j])-sd(space_betap[n,,j])*2,mean(space_betap[n,,j])+sd(space_betap[n,,j])*2)#calculatiung confidence intervals
}

##determine if 0 is within 95% credibility interval 
if((bounds[2,1]>0 & bounds[2,2]>0) | (bounds[2,1]<0 & bounds[2,2]<0)) #if distribution doesn't signifcantly contain 0
{
Longhurst[which(Longhurst==j)] <- mean(betap[2,,j])
}else{
Longhurst[which(Longhurst==j)] <- NA  
}

if((bounds_space[2,1]>0 & bounds_space[2,2]>0) | (bounds_space[2,1]<0 & bounds_space[2,2]<0)) #if distribution doesn't signifcantly contain 0
{
Longhurst_space[which(Longhurst_space==j)] <- mean(space_betap[2,,j])
}else{
Longhurst_space[which(Longhurst_space==j)] <- NA  
}


#RMSE (normalised to mean of each regions observations--allowing comparison between regions as well as within)
RMSE_space <- mean(sqrt((space_fittedChl-chl)^2),na.rm=T)/mean(chl,na.rm=T)
RMSE <-  mean(sqrt((fittedChl-chl)^2),na.rm=T)/mean(chl,na.rm=T)

#table of results
ResultsTab_space[indio,] <- c(indio,mean(space_betap[2,,j]),bounds_space[2,],RMSE_space)
ResultsTab[indio,] <- c(indio,mean(betap[2,,j]),bounds[2,],RMSE)
Chlcontent[indio] <- mean(chl,na.rm=T)*Area_values[ind_AV]
Test[indio] <- Area_values[ind_AV]
trend_betap2[,indio] <- betap[2,,j]#restructure betap array for use in violin plot
trend_betap2_space[,indio]  <- space_betap[2,,j]

#scatter for space fitted vs obs
    savename <- paste0("~/Ch1-scripts/Comparison_1500/",indio,"_space_scatter.png")
     
    png(savename, 
    type="cairo",
    units="in", 
    width=2.76, 
    height=2.76, 
    pointsize=12, 
    res=600)
    par(mar=c(3,3,1.2,1),mgp=c(1.75,0.2,0),cex.lab=1.5,cex.axis=1.15,cex.main=1.5,cex.sub=1.5)
plot(c(chl),c(space_fittedChl),xlab="Observed chl",
     ylab="Modelled chl",family="serif",
          ylim=c(min(c(chl,space_fittedChl),na.rm=T),max(c(chl,space_fittedChl),na.rm=T)),
     xlim=c(min(c(chl,space_fittedChl),na.rm=T),max(c(chl,space_fittedChl),na.rm=T)),
     main=indio,tck=0.01)
  abline(0,1,col="red")
  abline(lm(c(space_fittedChl)~c(chl)),col="blue")
dev.off()

    
#scatter for no space fitted vs obs
    savename <- paste0("~/Ch1-scripts/Comparison_1500/",indio,"_no_space_scatter.png")
    
    png(savename, 
    type="cairo",
    units="in", 
    width=2.76, 
    height=2.76, 
    pointsize=12, 
    res=600)
    par(mar=c(3,3,1.2,1),mgp=c(1.75,0.2,0),cex.lab=1.5,cex.axis=1.15,cex.main=1.5,cex.sub=1.5)
plot(c(chl),c(fittedChl),xlab="Observed chl",
     ylab="Modelled chl",family="serif",
          ylim=c(min(c(chl,fittedChl),na.rm=T),max(c(chl,fittedChl),na.rm=T)),
     xlim=c(min(c(chl,fittedChl),na.rm=T),max(c(chl,fittedChl),na.rm=T)),
     main=indio,tck=0.01)
  abline(0,1,col="red")
  abline(lm(c(fittedChl)~c(chl)),col="blue")
dev.off()

#distributions of trend value
savename <- paste0("~/Ch1-scripts/Comparison_1500/",indio,"_distribution.png")

    png(savename, 
    type="cairo",
    units="in", 
    width=2.76, 
    height=2.76, 
    pointsize=12, 
    res=600)
    par(mar=c(3,3,1.2,1),mgp=c(1.75,0.2,0),cex.lab=1.5,cex.axis=1.25,cex.main=1.5,cex.sub=1.5)
plot(density(betap[2,,j]),col="blue",xlab=expression(paste("Trend  ", "(%yr"^" -1",")")),
     ylab="Probability density",family="serif",
     xlim=c((min(c(space_betap[2,,j],betap[2,,j])-0.1)),(max(c(betap[2,,j],space_betap[2,,j])+0.1))),
     main=indio,lwd=2,tck=0.01)
  lines(density(space_betap[2,,j]),lwd=2)
dev.off()
  
###region fit Comparison

  regave[,j]<-apply(chl,3,mean,na.rm=T)#average time series for region
regave_space[,j] <- apply(space_fittedChl,3,mean,na.rm=T)#average time series for region
regave_nospace[,j] <- apply(fittedChl,3,mean,na.rm=T)#average time series for region


yearaxis <- as.Date(seq(ISOdate(1997,9,1),ISOdate(2013,12,1),"month"))
 yeartick <-seq(as.Date("1997/9/1"),as.Date("2013/12/1"),"year")
 yeartick <- yeartick[-seq(1,17,2)] #every other year

savename <- paste0("~/Ch1-scripts/Comparison_1500/",indio,"_Fit_Comparison.png")
png(savename, 
    type="cairo",
    units="in", 
    width=8.27, 
    height=2.92, 
    pointsize=12, 
    res=600)
par(mar=c(1.2,4.3,1.2,1),mgp=c(1.75,0.2,0),cex.lab=1.5,cex.axis=1.25,cex.main=1.5,cex.sub=1.5)
plot(yearaxis,regave[,j],type='l',xlab="Time (m)",lwd=2,family="serif",
     ylab=expression(paste("Chl  ", "(mg m"^"-3",")")),xaxt='n',tck=0.01,
     ylim=c(0,max(regave[,j]))+0.05)  #average time series for region
axis.Date(1,at=yeartick,tck=0.01,family='serif')
lines(yearaxis,regave_nospace[,j],type='l',col='blue',lwd=2)      #no space model acerage output for region overlaid
lines(yearaxis,regave_space[,j],type='l',col='red',lwd=2)      #spatial model acerage output for region
dev.off()
#alternative options for paper:
#par(mar=c(1.2,3.4,1.2,1),mgp=c(1.75,0.2,0),cex.lab=1.5,cex.axis=1.25,cex.main=1.5,cex.sub=1.5,family='serif')

#     ylim=c(0,max(regave[,j]))+0.02)  #average time series for region
#legend("bottomright",legend=c("Observations","STC model","TC model"),lwd=c(2,2,2),col=c("black",'red','blue'),bty='n')  


}
}


#global map for space trend
    savename <- paste0("~/Ch1-scripts/Comparison_1500/space_global.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8.27, 
    height=6.69, 
    pointsize=12, 
    res=600)
    com <- rev(c("red3","red1","white","blue1","blue3"))
    par(mar=c(5.1,4.1,4.1,4.1),family="serif")
image.plot(x = lons ,y = lats, z = Longhurst_space,zlim=c(-2.11,+2.11),
           xlab="Longitude", ylab="Latitude",
           legend.line=3, family="serif",
           col=designer.colors( n=256, col= com, x=
                                  seq(0,1,, length(com)) ,alpha=1.0),legend.lab=expression(paste("Trend  ", "(%yr"^" -1",")")),yaxt='n',tck=0.01)
map(database="world",add=T,fill=T,col="gainsboro")
axis(2,at=c(-80,-40,0,40,80),tck=0.01)
text(-4,-3,"1");text(75,-2,"2");text(75,-23,"3");text(-43,19,"4");text(-107,10,"5");text(-147,23,"6");text(-130,-1,"7");
text(-15,-24,"8");text(+163,2,"9");text(-35,4,"10");text(-52,41,"11");text(148,38.5,"12");text(-25,50,"13");text(-18,35,"14");
text(-52,32,"15");text(-160,39.5,"16");text(160,24,"17");text(-152,50.5,"18");text(170,48.5,"19");text(-130,-22.5,"20");
text(-133,-37.5,"21");text(75,-35.5,"21");text(0,-48,"22");text(163,-36,"23");
text(163,-36,"23");
dev.off()

#global map for no space trend
    savename <- paste0("~/Ch1-scripts/Comparison_1500/no_space_global.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=8.27, 
    height=6.69, 
    pointsize=12, 
    res=600)
    par(mar=c(5.1,4.1,4.1,4.1),family="serif")
    image.plot(x = lons ,y = lats, z = Longhurst,zlim=c(-2.11,+2.11),
           xlab="Longitude", ylab="Latitude",
           legend.line=3, family="serif",
           col=designer.colors( n=256, col= com,x=
                                  seq(0,1,, length(com)) ,alpha=1.0), legend.lab=expression(paste("Trend  ", "(%yr"^" -1",")")),yaxt='n',tck=0.01)
map(database="world",add=T,fill=T,col="gainsboro")
axis(2,at=c(-80,-40,0,40,80),tck=0.01)
text(-4,-3,"1");text(75,-2,"2");text(75,-23,"3");text(-43,19,"4");text(-107,10,"5");text(-147,23,"6");text(-130,-1,"7");
text(-15,-24,"8");text(+163,2,"9");text(-35,4,"10");text(-52,41,"11");text(148,38.5,"12");text(-25,50,"13");text(-18,35,"14");
text(-52,32,"15");text(-160,39.5,"16");text(160,24,"17");text(-152,50.5,"18");text(170,48.5,"19");text(-130,-22.5,"20");
text(-133,-37.5,"21");text(75,-35.5,"21");text(0,-48,"22");text(163,-36,"23");
text(163,-36,"23");
dev.off()



save(ResultsTab,ResultsTab_space,Chlcontent,trend_betap2,trend_betap2_space,file="Results_tables.Rdata")
