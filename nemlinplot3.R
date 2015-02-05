#different in the nemlinearity

nemlinplot<-function(y.skala,mit,   xfel, yfel,mitmain){

  RanMax<-max(abs(range(mit)))
  xPow<-3
  skal<-seq(0,1,by=0.01)^xPow
  brakes<-c(-rev(skal),skal[-1])*RanMax
  
  
##x.skala<-c(1:dim(mit)[2])
x.skala<-c(1:dim(mit)[2]/adatsec)
#nemlineris skálás
image( x.skala,y.skala,t(mit),xlab=xfel, ylab=yfel, main=mitmain,col=rainbow(200, start=0,end=0.7),breaks=brakes,cex.lab=1.2,cex.main=1.2, yaxt='n')
##axis(1,c(1:dim(mit)[2])/adatsec, tick='FALSE')

#csatszamok<-c(cs:1)
##axis(2,round(y.skala[1:length(y.skala)]),abs(round(y.skala[length(y.skala):1])))
##axis(2,round(y.skala[1:length(y.skala)]),abs(round(y.skala[length(y.skala):1])))
##     at=c(0,seq(range(y.skala)[1],range(y.skala)[2],length.out=5)))
axis(2,at=c(0,seq(range(y.skala)[1],range(y.skala)[2],length.out=5)))
image.plot(x.skala,y.skala,t(mit),col=rainbow(200, start=0,end=0.7),breaks=brakes, add=TRUE, legend.shrink=0.9, 
legend.width=0.7,horizontal =FALSE,cex=1.2)
##axis(1,c(1:dim(mit)[2])/20, tick='FALSE')
##axis(2,round(y.skala[1:length(y.skala)]),abs(round(y.skala[length(y.skala):1])))
}
