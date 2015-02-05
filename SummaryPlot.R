#SummaryPlot.R



legnev<-paste("legendreRD_csat_",ch,"_k_", k,".png", sep = "")
CellNames<-c(CellNames,legnev)

##png(pointsize=bm,filename = legnev, width = 1000, height = 800)
png(pointsize=bm,filename = legnev, width = 1200, height = 1000)
### Note: increase width and height parameters to obtain better resolution in png file
### Might have to change font size: bm
##par(mfrow=c(2,2),mar=c(5, 5, 4, 5) ,oma = c(0, 0, 2, 0) )


###par(mfrow=c(3,2) ,oma = c(0, 0, 2, 0) )
par(mfrow=c(2,2),oma = c(1, 1, 1, 1))

#feszultsegimage
##mit<--DATLAG #mit ábrázolunk?
mit<-DATLAG                    ## Plotting the avg of the extracellular potentials
##mit<-mit[thal[1]:thal[32],]    ## Plotting only the thalamus channels
mit<-mit[(ch2-(dipolcsat-1)): (ch2+(dipolcsat-1)) ,]
mit<-interpol(el.poz,mit,felbontas)   ###  Also interpolate the extracellular potentials
mit<-mit[nrow(mit):1,]                                  
##xnev<-'data points'
##ynev<-'Channels'
xnev<-'time (ms)'
ynev<-expression(paste('location ','(',mu,plain(m),')'))
fonev<-paste('Averaged extracellular potential (ch',ch,')')
###fonev<-c(ch,min(DATLAG[ch,]),ch2,min(DATLAG[ch2,]))
##y.skala<-c(1:dim(mit)[1])
##y.skala<-el.poz
y.skala<-felbontas
nemlinplot(y.skala,mit,xnev, ynev, fonev)

#ITRAD.ip<-interpol(el.poz,ITRAD[(ch2-(dipolcsat-1)): (ch2+(dipolcsat-1)) ,],felbontas)
ITRAD.ip<-interpol(el.poz,ITRAD[(ch2-(dipolcsat-1)-a+1): (ch2+(dipolcsat-1)-a+1) ,],felbontas)

##mit<- -ITRAD                     ## Plotting the traditional CSD (the negative of the 2nd spatial derivatives)
mit<- -ITRAD.ip                     ## Plotting the traditional CSD (the negative of the 2nd spatial derivatives)
##mit<-mit[thal[1]:thal[32],]   ## Plotting only the thalamus channels
##mit<-mit[(ch2-(dipolcsat-1)): (ch2+(dipolcsat-1)) ,]
mit<-mit[nrow(mit):1,]       ## reverse rows since image plot starts plotting at bottom left of matrix
##xnev<-'data points'
xnev<-'time (ms)'
##ynev<-'Channels'
###ynev<-expression(paste('location ','(',mu,plain(m),')'))
ynev<-''
fonev<-'Traditional CSD'
##y.skala<-c(1:dim(mit)[1])
y.skala<-felbontas
nemlinplot(y.skala,mit,xnev, ynev, fonev)

#transzfermátrix
#mit<-T.dirac#mit ábrázolunk?
#mit<-mit[nrow(mit):1,]
#xnev<-'tengely'
#ynev<-'tengely'
#fonev<-'transzfermátrix'
#nemlinplot(mit,xnev, ynev, fonev)

##plot(S,t='l',ylim=c(0,1),main=c(tav,tav,tav,tavhej,tav3,tav3hej,tav4,tav4hej), xlab='distance')
###plot(S,t='l',ylim=c(0,1),main=c(Distances[tav],Distances[tavhej],Distances[tav3],Distances[tav3hej],Distances[tav4],Distances[tav4hej]), xlab='distance')
plot(S,t='l',ylim=c(0,1),main='Regularization function', xlab='distance')
###lines(Shej,col='BROWN')
##lines(S2,col='RED')
###lines(S3,col='BLUE')
###lines(S3hej,col='TURQUOISE')
lines(S4,col='RED')
###lines(S4hej,col='MAGENTA')
###legend("topright",c(paste(Distances[tav],"Sdirac"),paste(Distances[tavhej],"Shej"),paste(Distances[tav3],"Sdirac stdCSD"),paste(Distances[tav3hej],"Shej stdCSD"),paste(Distances[tav4],"Sdirac stdA0"),paste(Distances[tav4hej],"Shej stdA0")),text.col=c("BLACK","BROWN","BLUE","TURQUOISE","RED","MAGENTA"))
legend("topright",c(paste(Distances[tav],"S1"),paste(Distances[tav4],"S2")),text.col=c("BLACK","RED"))


#feszultsegimage
##mit<--I #mit ábrázolunk?
mit<- I #mit ábrázolunk?  ## Removed the minus sign
mit<-mit[nrow(mit):1,]
xnev<-'time (ms)'
ynev<-expression(paste('location ','(',mu,plain(m),')'))
##ynev<- 'Radius'
###fonev<-'I_dirac'
fonev<-'sCSD (dirac)'
hejvas<-numeric()
#hejvas<-c(-(legen$hejvas[(length(legen$hejvas)-1):1]),0,legen$hejvas[1:(length(legen$hejvas)-1)])
#nemlinplot(hejvas,mit,xnev, ynev, fonev)
nemlinplot(felbontas,mit,xnev, ynev, fonev)

##mit<- Ihej #mit ábrázolunk?  ## graph the hej version as well
##mit<-mit[nrow(mit):1,]
##xnev<-'data points'
##ynev<-'Radius'
##fonev<-'I_hej'
##hejvas<-numeric()
#hejvas<-c(-(legen$hejvas[(length(legen$hejvas)-1):1]),0,legen$hejvas[1:(length(legen$hejvas)-1)])
#nemlinplot(hejvas,mit,xnev, ynev, fonev)
##nemlinplot(felbontashej,mit,xnev, ynev, fonev)

mit<- IhejDir #mit ábrázolunk?  ## Removed the minus sign
##mit<- Idirac4 #mit ábrázolunk?  ## Removed the minus sign
mit<-mit[nrow(mit):1,]
##xnev<-'data points'
xnev<-'time (ms)'
ynev<-expression(paste('location ','(',mu,plain(m),')'))
##ynev<-'Radius'
fonev<-'I_hej using Dirac tav'
##fonev<-'I_dirac using stddev A0A1 method'
hejvas<-numeric()
#hejvas<-c(-(legen$hejvas[(length(legen$hejvas)-1):1]),0,legen$hejvas[1:(length(legen$hejvas)-1)])
#nemlinplot(hejvas,mit,xnev, ynev, fonev)
###nemlinplot(felbontas,mit,xnev, ynev, fonev)

#Splotmatrix<-matrix(c(S,Shej,),ncol=4)
#matplot(Distances,Imaxesmatrix,t='l',main=c(Imaxes[tav],Imaxeshej[tavhej]), col=c('BLACK','RED','BROWN', 'BLUE'),sub=d,xlab='distance')






##plot(I1[imaxhol[1],],t='l',main=c(I1[imaxhol],imaxhol[1],max(abs(I1[dipolcsat, ])),dipolcsat))
##plot(I1[dipolcsat,],t='l',main=c(I1[imaxhol],imaxhol[1],max(abs(I1[dipolcsat, ])),dipolcsat))
##lines(I1[dipolcsat-1,], col='BLUE')
##lines(I1[dipolcsat+1,], col='RED')
###matplot(t(I1[(dipolcsat-1):(dipolcsat+2),]),t='l',col=c('BLUE','BLACK','BLACK','RED'),main=c(I1[imaxhol],imaxhol[1],max(abs(I1[dipolcsat, ])),dipolcsat),xlab='data points')


###plot(Ihej1[dipolcsathej,],t='l',main=c(Ihej1[imaxholhej],imaxholhej[1],max(abs(Ihej1[dipolcsathej, ])),dipolcsathej))
###lines(Ihej1[dipolcsathej-1,], col='RED')
###lines(Ihej1[dipolcsathej+1,], col='BLUE')

#paste("csat",ch,"k",k,".png",sep="")


dev.off()

########################################################################


legnev<-paste("SummarylegendreRD_csat_",ch,"_k_", k,".png", sep = "")
CellNames<-c(CellNames,legnev)

##png(pointsize=bm,filename = legnev, width = 1000, height = 800)
png(pointsize=bm,filename = legnev, width = 1200, height = 1000)
### Note: increase width and height parameters to obtain better resolution in png file
### Might have to change font size: bm
##par(mfrow=c(2,2),mar=c(5, 5, 4, 5) ,oma = c(0, 0, 2, 0) )


###par(mfrow=c(3,2) ,oma = c(0, 0, 2, 0) )
par(mfrow=c(2,2),oma = c(1, 1, 1, 1))

#feszultsegimage
##mit<--DATLAG #mit ábrázolunk?
mit<-DATLAG                    ## Plotting the avg of the extracellular potentials
##mit<-mit[thal[1]:thal[32],]    ## Plotting only the thalamus channels
#mit<-mit[(ch2-(dipolcsat-1)): ( ,]
#mit<-interpol(c((1:32)*100,33*100+(1:32)*50),mit,felbontas)   ###  Also interpolate the extracellular potentials
#mit<-mit[nrow(mit):1,]                                  
##xnev<-'data points'
##ynev<-'Channels'
xnev<-'time (ms)'
ynev<-expression(paste('location ','(',mu,plain(m),')'))
fonev<-paste('Averaged extracellular potential (ch',ch,')')
###fonev<-c(ch,min(DATLAG[ch,]),ch2,min(DATLAG[ch2,]))
y.skala<-c(1:dim(mit)[1])
##y.skala<-el.poz
#y.skala<-c((1:32)*100,33*100+(1:32)*50)
nemlinplot(y.skala,mit,xnev, ynev, fonev)


#ITRAD.ip<-interpol(el.poz,ITRAD[(ch2-(dipolcsat-1)): (ch2+(dipolcsat-1)) ,],felbontas)


##mit<- -ITRAD                     ## Plotting the traditional CSD (the negative of the 2nd spatial derivatives)
mit<- -ITRAD                     ## Plotting the traditional CSD (the negative of the 2nd spatial derivatives)
##mit<-mit[thal[1]:thal[32],]   ## Plotting only the thalamus channels
##mit<-mit[(ch2-(dipolcsat-1)): (ch2+(dipolcsat-1)) ,]
#mit<-mit[nrow(mit):1,]       ## reverse rows since image plot starts plotting at bottom left of matrix
##xnev<-'data points'
xnev<-'time (ms)'
##ynev<-'Channels'
###ynev<-expression(paste('location ','(',mu,plain(m),')'))
ynev<-''
fonev<-'Traditional CSD'
y.skala<-c(1:dim(mit)[1])
#y.skala<-felbontas
nemlinplot(y.skala,mit,xnev, ynev, fonev)















#ITRAD.ip<-interpol(el.poz,ITRAD[(ch2-(dipolcsat-1)): (ch2+(dipolcsat-1)) ,],felbontas)
ITRAD.ip<-interpol(el.poz,ITRAD[(ch2-(dipolcsat-1)-a+1): (ch2+(dipolcsat-1)-a+1) ,],felbontas)

##mit<- -ITRAD                     ## Plotting the traditional CSD (the negative of the 2nd spatial derivatives)
mit<- -ITRAD.ip                     ## Plotting the traditional CSD (the negative of the 2nd spatial derivatives)
##mit<-mit[thal[1]:thal[32],]   ## Plotting only the thalamus channels
##mit<-mit[(ch2-(dipolcsat-1)): (ch2+(dipolcsat-1)) ,]
mit<-mit[nrow(mit):1,]       ## reverse rows since image plot starts plotting at bottom left of matrix
##xnev<-'data points'
xnev<-'time (ms)'
##ynev<-'Channels'
###ynev<-expression(paste('location ','(',mu,plain(m),')'))
ynev<-''
fonev<-'Traditional CSD'
##y.skala<-c(1:dim(mit)[1])
y.skala<-felbontas
nemlinplot(y.skala,mit,xnev, ynev, fonev)

#transzfermátrix
#mit<-T.dirac#mit ábrázolunk?
#mit<-mit[nrow(mit):1,]
#xnev<-'tengely'
#ynev<-'tengely'
#fonev<-'transzfermátrix'
#nemlinplot(mit,xnev, ynev, fonev)

##plot(S,t='l',ylim=c(0,1),main=c(tav,tav,tav,tavhej,tav3,tav3hej,tav4,tav4hej), xlab='distance')
###plot(S,t='l',ylim=c(0,1),main=c(Distances[tav],Distances[tavhej],Distances[tav3],Distances[tav3hej],Distances[tav4],Distances[tav4hej]), xlab='distance')
#plot(S,t='l',ylim=c(0,1),main='Regularization function', xlab='distance')
###lines(Shej,col='BROWN')
##lines(S2,col='RED')
###lines(S3,col='BLUE')
###lines(S3hej,col='TURQUOISE')
#lines(S4,col='RED')
###lines(S4hej,col='MAGENTA')
###legend("topright",c(paste(Distances[tav],"Sdirac"),paste(Distances[tavhej],"Shej"),paste(Distances[tav3],"Sdirac stdCSD"),paste(Distances[tav3hej],"Shej stdCSD"),paste(Distances[tav4],"Sdirac stdA0"),paste(Distances[tav4hej],"Shej stdA0")),text.col=c("BLACK","BROWN","BLUE","TURQUOISE","RED","MAGENTA"))



#feszultsegimage
##mit<--I #mit ábrázolunk?
mit<- I #mit ábrázolunk?  ## Removed the minus sign
mit<-mit[nrow(mit):1,]
xnev<-'time (ms)'
ynev<-expression(paste('location ','(',mu,plain(m),')'))
##ynev<- 'Radius'
###fonev<-'I_dirac'
fonev<-'sCSD (dirac)'
hejvas<-numeric()
#hejvas<-c(-(legen$hejvas[(length(legen$hejvas)-1):1]),0,legen$hejvas[1:(length(legen$hejvas)-1)])
#nemlinplot(hejvas,mit,xnev, ynev, fonev)
nemlinplot(felbontas,mit,xnev, ynev, fonev)
legend("topright",c(paste(Distances[tav],"S1"),paste(Distances[tav4],"S2")),text.col=c("BLACK","RED"))
##mit<- Ihej #mit ábrázolunk?  ## graph the hej version as well
##mit<-mit[nrow(mit):1,]
##xnev<-'data points'
##ynev<-'Radius'
##fonev<-'I_hej'
##hejvas<-numeric()
#hejvas<-c(-(legen$hejvas[(length(legen$hejvas)-1):1]),0,legen$hejvas[1:(length(legen$hejvas)-1)])
#nemlinplot(hejvas,mit,xnev, ynev, fonev)
##nemlinplot(felbontashej,mit,xnev, ynev, fonev)

mit<- IhejDir #mit ábrázolunk?  ## Removed the minus sign
##mit<- Idirac4 #mit ábrázolunk?  ## Removed the minus sign
mit<-mit[nrow(mit):1,]
##xnev<-'data points'
xnev<-'time (ms)'
ynev<-expression(paste('location ','(',mu,plain(m),')'))
##ynev<-'Radius'
fonev<-'I_hej using Dirac tav'
##fonev<-'I_dirac using stddev A0A1 method'
hejvas<-numeric()
#hejvas<-c(-(legen$hejvas[(length(legen$hejvas)-1):1]),0,legen$hejvas[1:(length(legen$hejvas)-1)])
#nemlinplot(hejvas,mit,xnev, ynev, fonev)
###nemlinplot(felbontas,mit,xnev, ynev, fonev)

#Splotmatrix<-matrix(c(S,Shej,),ncol=4)
#matplot(Distances,Imaxesmatrix,t='l',main=c(Imaxes[tav],Imaxeshej[tavhej]), col=c('BLACK','RED','BROWN', 'BLUE'),sub=d,xlab='distance')






##plot(I1[imaxhol[1],],t='l',main=c(I1[imaxhol],imaxhol[1],max(abs(I1[dipolcsat, ])),dipolcsat))
##plot(I1[dipolcsat,],t='l',main=c(I1[imaxhol],imaxhol[1],max(abs(I1[dipolcsat, ])),dipolcsat))
##lines(I1[dipolcsat-1,], col='BLUE')
##lines(I1[dipolcsat+1,], col='RED')
###matplot(t(I1[(dipolcsat-1):(dipolcsat+2),]),t='l',col=c('BLUE','BLACK','BLACK','RED'),main=c(I1[imaxhol],imaxhol[1],max(abs(I1[dipolcsat, ])),dipolcsat),xlab='data points')


###plot(Ihej1[dipolcsathej,],t='l',main=c(Ihej1[imaxholhej],imaxholhej[1],max(abs(Ihej1[dipolcsathej, ])),dipolcsathej))
###lines(Ihej1[dipolcsathej-1,], col='RED')
###lines(Ihej1[dipolcsathej+1,], col='BLUE')

#paste("csat",ch,"k",k,".png",sep="")


dev.off()






