nemlinplot<-function(y.skala,mit,   xfel, yfel,mitmain){
skalaz<-3 #7 #mennyire legyen nemlineáris
tores<-300 #hány töréspont leszem=színek szama +1

terj<-max(abs(mit))*1.01*2 # aszorzó azér kell hogy a szélső értékek heléyn ne fehét legye
f<-abs(seq(-1,1, length.out=(tores-1)))

o3<-rep(1,tores-1)

ba<-o3*exp(abs(f*skalaz))
inter<-rep(-max(abs(mit))*1.01,tores)

interval<-ba*terj/sum(ba)
##plot(interval)
for(i in 2:(tores)){
inter[i]<-inter[i-1]+interval[i-1]
}
##x.skala<-c(1:dim(mit)[2])
x.skala<-c(1:dim(mit)[2]/adatsec)
#nemlineris skálás
image( x.skala,y.skala,t(mit),xlab=xfel, ylab=yfel, main=mitmain,col=rainbow(tores-1),breaks=inter,cex.lab=1.2,cex.main=1.2, yaxt='n')
##axis(1,c(1:dim(mit)[2])/adatsec, tick='FALSE')

#csatszamok<-c(cs:1)
##axis(2,round(y.skala[1:length(y.skala)]),abs(round(y.skala[length(y.skala):1])))
##axis(2,round(y.skala[1:length(y.skala)]),abs(round(y.skala[length(y.skala):1])))
##     at=c(0,seq(range(y.skala)[1],range(y.skala)[2],length.out=5)))
axis(2,at=c(0,seq(range(y.skala)[1],range(y.skala)[2],length.out=5)))
image.plot(x.skala,y.skala,t(mit),col=rainbow(tores-1, start=0,end=0.7),breaks=inter, add=TRUE, legend.shrink=0.9, 
legend.width=0.7,horizontal =FALSE,cex=1.2)
##axis(1,c(1:dim(mit)[2])/20, tick='FALSE')
##axis(2,round(y.skala[1:length(y.skala)]),abs(round(y.skala[length(y.skala):1])))
}
