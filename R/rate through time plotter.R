#relative rate through time plotter:

library(pbapply)
library(BTRTools)
library(ggplot2)
library(phytools)
#tree
treeA<-read.nexus(file="D:/BayesTraitsV2/tree250.nex")
#treeA<-read.nexus("D:/BayesTraitsV2/hackett250.nex")
#Post prob data
#rjpp.output<-rjpp("D:/BayesTraitsV2/rostrumpcsscaledhack.txt.PP.txt","D:/BayesTraitsV2/rostrumpcsscaledhack.txt.PP.trees",treeA)
rjpp.output<-rjpp("D:/BayesTraitsV2/braincasepcsscaledkappa.txt.PP.txt","D:/BayesTraitsV2/braincasepcsscaledkappa.txt.PP.trees",treeA)
#rjpp.output<-rjpp("D:/BayesTraitsV2/rostrumpcsscaledelt.txt.PP.txt","D:/BayesTraitsV2/rostrumpcsscaleddelt.txt.PP.trees",treeA)

rttplotter<-function(rjpp.output,treeA){

pprates<-rjpp.output$data$meanRate[-1]

scaled.edges<-rjpp.output$data$meanBL[-1]/rjpp.output$data$orgBL[-1]
scaled.25<-rjpp.output$data$quart25[-1]/rjpp.output$data$orgBL[-1]
scaled.75<-rjpp.output$data$quart75[-1]/rjpp.output$data$orgBL[-1]

H1<-nodeHeights(treeA)
timedepth<-round(max(H1))
rate.at.time<-vector()
range.at.time<-vector()
twentyfifth.at.time<-vector()
seventyfifth.at.time<-vector()

for (i in 1:timedepth){
  #slice at time of timebin:
  spot<-i
  edges<-which(H1[,2]>spot&H1[,1]<spot)
  rate.at.time[i]<-mean(scaled.edges[edges])
  twentyfifth.at.time[i]<-mean(scaled.25[edges])
  seventyfifth.at.time[i]<-mean(scaled.75[edges])
  
}



df2<-data.frame(mya = as.numeric(c(-(timedepth-1):-1)), rate = as.numeric(rate.at.time[-timedepth]), top25=twentyfifth.at.time[-timedepth], top75=seventyfifth.at.time[-timedepth])

z <- ggplot(df2, aes(x = mya, y = rate)) + geom_line(color = "dodgerblue",size=2) + geom_ribbon(aes(ymin=top25, ymax=top75, x=mya), fill = "blue", alpha = 0.3)
z
}

rttplotter<-function(rjpp.output,treeA){
  
  pprates<-rjpp.output$data$meanRate[-1]
  
  scaled.edges<-rjpp.output$data$meanBL[-1]/rjpp.output$data$orgBL[-1]
  scaled.25<-rjpp.output$data$quart25[-1]/rjpp.output$data$orgBL[-1]
  scaled.75<-rjpp.output$data$quart75[-1]/rjpp.output$data$orgBL[-1]
  
  H1<-nodeHeights(treeA)
  timedepth<-round(max(H1))
  rate.at.time<-vector()
  range.at.time<-vector()
  twentyfifth.at.time<-vector()
  seventyfifth.at.time<-vector()
  
  for (i in 1:timedepth){
    #slice at time of timebin:
    spot<-i
    edges<-which(H1[,2]>spot&H1[,1]<spot)
    rate.at.time[i]<-mean(scaled.edges[edges])
    twentyfifth.at.time[i]<-mean(scaled.25[edges])
    seventyfifth.at.time[i]<-mean(scaled.75[edges])
    
  }
  
  
  
  df2<-data.frame(mya = as.numeric(c(-(timedepth-1):-1)), rate = as.numeric(rate.at.time[-timedepth]), top25=twentyfifth.at.time[-timedepth], top75=seventyfifth.at.time[-timedepth])
  
  z <- ggplot(df2, aes(x = mya, y = rate)) + geom_line(color = "dodgerblue",size=2) + geom_ribbon(aes(ymin=top25, ymax=top75, x=mya), fill = "blue", alpha = 0.3)
  z
}


rttplotter<-function(rjpp.output,treeA){
  
  pprates<-rjpp.output$data$meanRate[-1]
  
  scaled.edges<-rjpp.output$data$meanBL[-1]/rjpp.output$data$orgBL[-1]
  scaled.25<-rjpp.output$data$quart25[-1]/rjpp.output$data$orgBL[-1]
  scaled.75<-rjpp.output$data$quart75[-1]/rjpp.output$data$orgBL[-1]
  
  H1<-nodeHeights(treeA)
  timedepth<-round(max(H1))
  rate.at.time<-vector()
  range.at.time<-vector()
  twentyfifth.at.time<-vector()
  seventyfifth.at.time<-vector()
  
  for (i in 1:timedepth){
    #slice at time of timebin:
    spot<-i
    edges<-which(H1[,2]>spot&H1[,1]<spot)
    rate.at.time[i]<-mean(scaled.edges[edges])
    twentyfifth.at.time[i]<-mean(scaled.25[edges])
    seventyfifth.at.time[i]<-mean(scaled.75[edges])
    
  }
  
  
  
  df2<-data.frame(mya = as.numeric(c(-(timedepth-1):-1)), rate = as.numeric(rate.at.time[-timedepth]), top25=twentyfifth.at.time[-timedepth], top75=seventyfifth.at.time[-timedepth])
  
  z <- ggplot(df2, aes(x = mya, y = rate)) + geom_line(color = "dodgerblue",size=2) + geom_ribbon(aes(ymin=top25, ymax=top75, x=mya), fill = "blue", alpha = 0.3)
  z
}

rttplotter2<-function(rjpp.output,treeA,module.name){
  
  #pprates<-rjpp.output$data$meanRate[-1]/mean(rjpp.output$data$meanRate[-1])
  pprates<-rjpp.output$data$meanRate[-1]
  
  #scaled.edges<-rjpp.output$data$meanBL[-1]/rjpp.output$data$orgBL[-1]
  #scaled.25<-rjpp.output$data$quart25[-1]/rjpp.output$data$orgBL[-1]
  #scaled.75<-rjpp.output$data$quart75[-1]/rjpp.output$data$orgBL[-1]
  
  H1<-nodeHeights(treeA)
  timedepth<-round(max(H1))
  rate.at.time<-vector()

  
  for (i in 1:timedepth){
    #slice at time of timebin:
    spot<-i
    edges<-which(H1[,2]>spot&H1[,1]<spot)
    rate.at.time[i]<-mean(pprates[edges])
    
  }
  rate.at.time<-rate.at.time[1:timedepth-1]/mean(rate.at.time[1:timedepth-1])
  
  
  
  df2<-data.frame(mya = as.numeric(c(-(timedepth-1):-1)), rate = as.numeric(rate.at.time[-timedepth]))
  colnames(df2)[2]<-module.name
  return(df2)
}


########################################
########################################
#here is the part where you actually plot
########################################
########################################
#rostrumres, vaultres, etc are the output you got from rjpp in the other 
#plotting script
r<-rttplotter2(rjpp.output = rostrumres, treeA = phy352, module.name = "Rostrum")
v<-rttplotter2(rjpp.output = vaultres, treeA = phy352, module.name = "Vault")
o<-rttplotter2(rjpp.output = occiputres, treeA = phy352, module.name = "Occipital")
p<-rttplotter2(rjpp.output = palateres, treeA = phy352, module.name = "Palate")
n<-rttplotter2(rjpp.output = narisres, treeA = phy352, module.name = "Naris")
sph<-rttplotter2(rjpp.output = sphenoidres, treeA = phy352, module.name = "Basisphenoid")
j<-rttplotter2(rjpp.output = jointres, treeA = phy352, module.name = "Jaw Joint")
w<-rttplotter2(rjpp.output = wholeres, treeA = phy352, module.name = "Whole Skull")

library(plyr)
library(dplyr)
df3<-join_all(list(r,v,sph,p,j,n,o,w), by="mya")
library(reshape2)
ratedata<-melt(df3, id.vars = "mya")

ggplot(ratedata, aes(x = mya+1, y = value, colour=variable)) + geom_line(size=1) +
  scale_color_manual(values=c(cbbPalette,"darkgrey")) +
  theme_classic()+
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = -65)+
  scale_x_continuous(breaks = seq(-70 , 0, 5))


  theme(legend.position='none')
  

