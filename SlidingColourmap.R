#This is a plotting tool to make matplot with different curves for each channel, like raw data and different types of filtering.
library(tkrplot)
setwd(parent)
fid<-file(fajlnev,'rb')
start<-1 
q<-1 
dts<-3

  seek(fid,where=((q-1)*cs*mintf*dts+start*cs*mintf)*2,origin="start",rw="read",)
adat<-readBin(fid, what='integer', size=2, n=cs*mintf*dts,endian="little",signed="TRUE")
adat<-matrix(adat,nrow=cs)
adat<-adat[csat.rend,]



tradCSD<-function(LFP,ElectrodeDist){
  ChNb<-dim(LFP)[1]
  tCSD<-array(0,c(dim(LFP)))
  for(chan in 1: ChNb){
    if(chan==1) tCSD[1,]<-(LFP[2,]-LFP[1,])/ElectrodeDist^2
    else if(chan==ChNb) tCSD[ChNb,]<-(LFP[ChNb-1,]-LFP[ChNb,])/ElectrodeDist^2
    else tCSD[chan,]<-(LFP[chan-1,]+LFP[chan+1,]-2*LFP[chan,])/ElectrodeDist^2
  }
  return(tCSD)
}




ScrollingPlot<-function(LFP, CSD){
  
  #a<-allapot
  tt <- tktoplevel()
  left <- tclVar(1/adatsec)
  oldleft <- tclVar(1/adatsec)
  right <- tclVar(2000)
  
  f1 <- function(){
    lleft <- as.numeric(tclvalue(left))
    rright <- as.numeric(tclvalue(right))
    x <- seq(lleft,rright,by=1)/adatsec
    
    par(mfrow=c(2,1), mar=c(1.5,1,0.5,0.5) ,oma=c(0.5,1,0.5,0.5) )
    #layout(matrix(c(1:4),4,1),widths=c(1,1,1,1),   	heights=c(3,2,2,1))
    image(x,c(1:dim(LFP)[1]),t(LFP[,lleft:rright]),zlim=c(-5000,5000),col=rainbow(100, start=0,end=0.7))
    image(x,c(1:dim(CSD)[1]), t(CSD[,lleft:rright]),zlim=c(-1,1),col=rainbow(100, start=0,end=0.7))
    
    
  }
  
  img <- tkrplot(tt, f1,hscale=2,vscale=1)
  
  f2 <- function(...){
    ol <- as.numeric(tclvalue(oldleft))
    tclvalue(oldleft) <- tclvalue(left)
    r <- as.numeric(tclvalue(right))
    tclvalue(right) <- as.character(r + as.numeric(...) - ol)
    tkrreplot(img)
  }
  
  f3 <- function(...){
    tkrreplot(img)
  }
  
  f4 <- function(...){
    ol <- as.numeric(tclvalue(oldleft))
    tclvalue(left) <- as.character(ol+2000)
    tclvalue(oldleft) <- as.character(ol+2000)
    r <- as.numeric(tclvalue(right))
    tclvalue(right) <- as.character(r+2000)
    tkrreplot(img)
  }
  
  s1 <- tkscale(tt, command=f2, from=1, to=dim(LFP)[2],
                variable=left, orient="horiz",label='left')
  s2 <- tkscale(tt, command=f3, from=1, to=dim(LFP)[2],
                variable=right, orient="horiz",label='right')
  b1 <- tkbutton(tt, text='->', command=f4)
  
  tkpack(img,s1,s2,b1) 
}

csds<-tradCSD(adat[s1,],100)
csdv<-tradCSD(adat[v1,],100)
csdtal<-tradCSD(adat[thal,],50)

tCSD<-rbind(csds,csdv,csdtal)
ScrollingPlot(adat, tCSD)
