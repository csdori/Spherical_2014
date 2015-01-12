#This is a plotting tool to make matplot with different curves for each channel, like raw data and different types of filtering.
library(tcltk)
library(tkrplot)

ScrollingPlot<-function(data1,data2,data3){
 # data1<-adat[1:3,]
  #data2<-adat.filt.slow[1:3,]
  #data3<-adat.filt.mua[1:3,]
  dimData<-dim(data1)[1]
  #a<-allapot
  tt <- tktoplevel()
  left <- tclVar(1)
  oldleft <- tclVar(1)
  right <- tclVar(2000)
  y<-1:dim(data1)[2]
    
  f1 <- function(){
    lleft <- as.numeric(tclvalue(left))
    rright <- as.numeric(tclvalue(right))
    x <- seq(lleft,rright,by=1)/adatsec
    par(mfrow=c(dimData,1), mar=c(1.5,1,0.5,0.5) ,oma=c(0.5,1,0.5,0.5) )
    #layout(matrix(c(1:4),4,1),widths=c(1,1,1,1),   	heights=c(3,2,2,1))
    for(pl in 1:dimData){
      #cat("A dimenzio:" , dim(t(rbind(data1[pl,],data2[pl,],data3[pl,]))))
      matplot( x,t(rbind(data1[pl,lleft:rright],data2[pl,lleft:rright],data3[pl,lleft:rright])),t='l')
    }
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
  
  s1 <- tkscale(tt, command=f2, from=1, to=length(y),
                variable=left, orient="horiz",label='left')
  s2 <- tkscale(tt, command=f3, from=1, to=length(y),
                variable=right, orient="horiz",label='right')
  b1 <- tkbutton(tt, text='->', command=f4)
  
  tkpack(img,s1,s2,b1) 
}


#ScrollingPlot(adat[c(5,17,53),],adat.filt.gamma[c(5,17,53),],adat.filt.mua[c(5,17,53),])
ScrollingPlot(adat[1:3,],adat[1:3,],adat.filt.mua[1:3,])
ScrollingPlot(adat[1:5,],adat.filt.slow[1:5,],adat.filt.mua[1:5,])
timewindow<-c(1:100000)
whichCh<-110:12
ScrollingPlot(adat[whichCh,],adat[whichCh,],adat.filt.mua[whichCh,])
ScrollingPlot(adat[whichCh,timewindow],adat.filt.slow[whichCh,timewindow],adat.filt.slow[whichCh,timewindow]*1)
ScrollingPlot(adat[whichCh,],adat[whichCh,],10*abs(adat.filt.mua[whichCh,]))

