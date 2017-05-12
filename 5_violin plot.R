library(ggplot2)
library(grid)

rm(list=ls())
setwd("~/Ch1-scripts/Comparison_1500")

load("Results_tables.Rdata")
##create data frame with each iteration of trend from spatial and non spatial models
 Trend <- c(rbind(trend_betap2_space,trend_betap2))
  corr <- rep(c(rep("STC",each=40000),rep("TC",each=2000)),times=23)
  

  Region <- rep(c(1:23),each=42000)
 
  daf <- cbind(Region,Trend,corr)
  daf <- as.data.frame(daf)
  daf[,2] <- Trend#error converting to data frame ledas to factorisation
  daf$Region <- as.factor(Region)
  daf$corr <- as.factor(daf$corr)   



  
  
 #create violin plot  
       savename <- paste0("~/Ch1-scripts/Violins.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=7, 
    height=8.27, 
    pointsize=12, 
    res=600)
      p <- ggplot(daf,aes(x=Region,y=Trend,fill=corr))
    p+geom_hline(yintercept=-0.38,colour="tan2",size=0.9)+geom_hline(yintercept=-0.036,colour="slateblue",size=0.9)+geom_violin(adjust=2,scale="width")+
      scale_fill_manual(values=c("slateblue","tan2"))+coord_flip()+ 
    theme_bw()+ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + guides(fill=guide_legend(title=NULL))+
      theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),text=element_text(family="Times New Roman",size=16))+
      scale_y_continuous(breaks=seq(-2,2,1),minor_breaks = seq(-2,2,1),limits=c(-2.4,2.4))     
     
      # theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),axis.line=element_line(size=0.72),text=element_text(size=10,family="Times New Roman"))+
      #scale_y_continuous(breaks=seq(-2,2,1),minor_breaks = seq(-2,2,1),limits=c(-2.4,2.4))

  dev.off()

  #RMSE bar chart
  dafbar <- rbind(ResultsTab_space,ResultsTab)
  corr <- c(rep("STC",each=23),rep("TC",each=23))
  dafbar <- cbind(dafbar,corr)
  dafbar <- as.data.frame(dafbar)
  dafbar$corr <- as.factor(dafbar$corr)
 dafbar$NRMSE <- c(ResultsTab_space[,5],ResultsTab[,5])#weird error converting to data frame ledas to factorisation
  dafbar$Region <- rep(c(1:23),times=2)
  dafbar$Region <- as.factor(dafbar$Region)  
  
         savename <- paste0("~/Ch1-scripts/Violin_bar.png")
    png(savename, 
    type="cairo",
    units="in", 
    width=3, 
    height=8.27, 
    pointsize=12, 
    res=600)
      p <- ggplot(dafbar,aes(x=Region,y=NRMSE,fill=corr))
  p+geom_bar(position="dodge",stat="identity")+coord_flip()+
    scale_fill_manual(values=c("slateblue","tan2"))+ theme_bw()+
     guides(fill=guide_legend(title=NULL))+theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),text=element_text(family="Times New Roman",size=16))
    #guides(fill=guide_legend(title=NULL))+ggtitle("RMSE per region")+theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),axis.line=element_line(size=0.72),text=element_text(size=10,family="Times New Roman"))
  dev.off()
  

  ##two are later stitched together in Illustrator