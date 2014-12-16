#This is a plotting tool to make matplot with different curves for each channel, like raw data and different types of filtering.


ScrollingPlot<-function(data1,data2,data3){
  dimData<-dim(data1)[1]
  #a<-allapot
  tt <- tktoplevel()
  left <- tclVar(1/adatsec)
  oldleft <- tclVar(1/adatsec)
  right <- tclVar(2000)
  
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


ScrollingPlot(adat[1:3,],adat.filt.gamma[1:3,],adat.filt.mua[1:3,])
