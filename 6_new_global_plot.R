library(rgdal)
library(ggplot2)
library(mapproj)
library(munsell)
rm(list=ls())
setwd("~/ch2/PLots/")

##load data, map files and setup fonts
windowsFonts(Times=windowsFont("TT Times New Roman"))
load("~/Ch1-scripts/Comparison_1500/Results_tables.Rdata")
long<-readOGR(dsn="C:/Users/mh23g14/Documents/SOCCOM/Map_data", layer="Longhurst_world_v4_2010")
long_df <- fortify(long)
wmap<-readOGR(dsn="C:/Users/mh23g14/Documents/SOCCOM/Map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea

#trend import and set trends where 0 with in credibility interval to equal 0 
trend  <- ResultsTab_space[,2]
for(i in 1:23){
if((ResultsTab_space[i,3]>0 & ResultsTab_space[i,4]>0) | (ResultsTab_space[i,3]<0 & ResultsTab_space[i,4]<0)) #if distribution doesn't signifcantly contain 0
{
}else{
trend[i] <- 0 
}
}
long_df$trend <- NA
#sorting differing region orders(stephs longhurst --> marineplan longhurst order-->our provnice order)
provsorder <- c(NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA,NA)
for(i in 1:54){
long_df$trend[which(long_df$id==i)] <- trend[provsorder[i]]
}

#centrepoints for region labelling
long_df$lonc <- NA
long_df$latc <- NA
centroids_df <- as.data.frame(coordinates(long))

 centroids_df[5,] <- c(-55.3,40.7) #small number of regions centrepoints not optimal
 centroids_df[33,] <- c(149.3,39) #small number of regions centrepoints not optimal
  centroids_df[36,] <- c(160.6,-36.3)
  centroids_df[51,] <- c(75,-36.2)
  for(i in c(4,5,6,7,8,9,10,18,22,23,31,32,33,34,35,36,37,38,39,40,41,51,52)){
long_df$lonc[which(long_df$id==i)] <- centroids_df$V1[i]
long_df$latc[which(long_df$id==i)] <- centroids_df$V2[i]
}
#correct labels
long_df$truid <- NA
provsorder <- c(NA,NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA)#slightly different due to null region

for(i in 1:54){
long_df$truid[which(long_df$id==i)] <- provsorder[i]
}
###create plot
com <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette
p <-  ggplot( data=long_df, aes(x=long, y=lat, group = group,fill=trend)) +
 scale_fill_gradientn(colours=com,na.value="black",limits=c(-2.1,2.1))+
  geom_polygon(colour='black',size=0.25)+ #longhurst outlines with half width size
  geom_text(data=long_df,aes(label = truid, x = lonc, y = latc,family="Times"),size=4) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 )+
   coord_equal() + guides(fill=guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")"))))+
  ylab("Latitude")+xlab("Longitude")+
   coord_cartesian(ylim = c(-75, 75),expand=F)+
scale_y_continuous(breaks=seq(-60,60,30))  +
  scale_x_continuous(breaks=seq(-180,180,60))  +
  annotate("text",x=-133,y=-38.1,label="21",size=4,colour='black',family="Times")+ #extra label for region 21 to show that it wraps
  theme_bw()+theme(text=element_text(family="Times"))

ggsave("space.png",  width=8.27, height=3.44, dpi=300)

####for model without spatial correlation
#data coming from a different save point (for some reason)
load("~/Ch1-scripts/Comparison_1500/no_space_results.Rdata")
total_betap <- total_betap[2,,]
total_betap <- apply(total_betap,2,mean)#note:no regions where trend unlikely in this model

#1:54 longhurst regions to 1:23 of our regions
trend <- array(NA,dim=c(1,54))
corc <- c(21,8,54,4,38,10,2,12,33,3,29,20,14,37,32,31,6,13,17,40,42,15,45,49,9,11,7,35,19,22,30,36,34,43,5,18,46,41,44,1,24,25,16,52,51,50,28,47,48,23,53,39,26,27)
for(i in 1:54)
  {
  trend[corc[i]] <- total_betap[i]*100*12
}
trend <-trend[-which(is.na(trend)==T)] #remove nas
long_df$trend <- NA
#sorting differing region orders
provsorder <- c(NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA,NA)
for(i in 1:54){
long_df$trend[which(long_df$id==i)] <- trend[provsorder[i]]
}

#centrepoints for region labelling
long_df$lonc <- NA
long_df$latc <- NA
centroids_df <- as.data.frame(coordinates(long))

 centroids_df[5,] <- c(-55.3,40.7) #small number of regions centrepoints not optimal
 centroids_df[33,] <- c(149.3,39) #small number of regions centrepoints not optimal
  centroids_df[36,] <- c(160.6,-36.3)
  centroids_df[51,] <- c(75,-36.2)
  for(i in c(4,5,6,7,8,9,10,18,22,23,31,32,33,34,35,36,37,38,39,40,41,51,52)){
long_df$lonc[which(long_df$id==i)] <- centroids_df$V1[i]
long_df$latc[which(long_df$id==i)] <- centroids_df$V2[i]
}
#correct labels
long_df$truid <- NA
provsorder <- c(NA,NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA)#slightly different due to null region

for(i in 1:54){
long_df$truid[which(long_df$id==i)] <- provsorder[i]
}

###create plot
com <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette
p <-  ggplot( data=long_df, aes(x=long, y=lat, group = group,fill=trend)) +
 scale_fill_gradientn(colours=com,na.value="black",limits=c(-2.1,2.1))+
  geom_polygon(colour='black',size=0.25)+ #longhurst outlines with half width size
  geom_text(data=long_df,aes(label = truid, x = lonc, y = latc,family="Times"),size=4) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 )+
   coord_equal() + guides(fill=guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")"))))+
  ylab("Latitude")+xlab("Longitude")+
   coord_cartesian(ylim = c(-75, 75),expand=F)+
scale_y_continuous(breaks=seq(-60,60,30))  +
  scale_x_continuous(breaks=seq(-180,180,60))  +
  annotate("text",x=-133,y=-38.1,label="21",size=4,colour='black',family="Times")+ #extra label for region 21 to show that it wraps
  theme_bw()+theme(text=element_text(family="Times"))

ggsave("nospace.png",  width=8.27, height=3.44, dpi=300)
