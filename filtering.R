



#SpikeSorting(fajlnev)
detekt<-function(x){
  #ave<-mean(x,na.rm=TRUE)
  ave<-mean(x,na.rm=TRUE)
  #histAmpl<-hist(adat.mua.rms[11,],plot=F, breaks=100)
  
  #histAmpl$counts<-log(histAmpl$counts)
  #plot(histAmpl,ylim=c(0,10000))
  #plot(histAmpl$density,t='l')
  hataratlepes<-seq(along=x)[x >= ave]
  #fitting 2 gaussians on the distributions
  
  
  return(hataratlepes)
}





SpikeSorting<-function(fajlnev){
library("signal")
library("clv")
## open the file for reading in binary mode
fid<-file(fajlnev,"rb")
#
#fontsize for figures
bm<-18 #betűméret az ábrákon
#mennyi időt olvasson be (sec)?
## time step 
dt<-10 #(s-ban,10-enként változtatható)
#how many times  
x<-1 #hányszor  
#sáváteresztő szűrő 
#Nyquist frequency
NyqFreq<-mintf/2
## bandpass filter lower and upper frequency limits
#for up&down
#gamma

#slow
#honnan.slow<-0.1/NyqFreq
meddig.slow<-5/NyqFreq

#gamma
honnan.gamma<-20/NyqFreq #alső határ #30
meddig.gamma<-100/NyqFreq #felső határ 

#MUA
honnan.mua<-500/NyqFreq #300
meddig.mua<-1500/NyqFreq #3000

setwd(mentes)  ## set working directory
dirname2<-paste(fajlnev,"_",start,'_','sec_est',sep='')  ## name a new directory

dir.create(dirname2) #csinálunk egy mappát!!!  ## create the directory

#################SZŰRÉS############################
#szűrés paraméterei
setwd(parent)


#################
wowtime<-vector("list",cs) ## create vector of length cs=65
spikes<-vector("list",cs)  ## create vector of length cs=65

wowtime1<-vector("list",cs)
spikes1<-vector("list",cs)
a<-numeric()  ## creates numeric variable (vector)
##########################################
for(q in 1:x){
  
  adat<-numeric()
  adat.filt.gamma<-numeric()
  adat.filt.mua<-numeric()
  adat.filt.slow<-numeric()
  #if(((x-1)dt+start)== ) ##ltp időszaka  
  
  
  seek(fid,where=((q-1)*cs*mintf*dt+start*cs*mintf)*2,origin="start",rw="read",)
  
  #a spike a teljes időszak közepén van 
  
  adat<-readBin(fid, what='integer', size=2, n=cs*mintf*dt,endian="little",signed="TRUE")
  adat<-matrix(adat,nrow=cs)
  adat<-adat[csat.rend,]
  
  #Butterworth filter
  
  
  cat("defining filtering frequencies")
  filter.slow<-butter(3,meddig.slow, type = "low")
  filter.gamma<-butter(3, c(honnan.gamma, meddig.gamma), type = "pass")
  filter.mua<-butter(3, c(honnan.mua, meddig.mua), type = "pass")
  #filter.gamma<-cheby1(5,3, W=c(honnan.gamma, meddig.gamma), type = "pass")
  #filter.mua<-butter(5,3, W=c(honnan.mua, meddig.mua), type = "pass")
  freqz(filter.gamma)
  adat.filt.slow<-t(apply(adat,1, function(x) filtfilt(filter.slow,x) ))
  adat.filt.gamma<-t(apply(adat,1, function(x) filtfilt(filter.gamma,x) ))
  adat.filt.mua<-t(apply(adat,1, function(x) filtfilt(filter.mua,x)))
  
  #adat.filt.slow.diff1<-t(apply(adat.filt.slow, 1,diff))
  
  #we should look for the up and down states on the sum of these filtered signals
  
  #matplot(t(adat.filt.gamma[1:50,1:9000]),t="l")
  #matplot(t(adat.filt.mua[1:5,1:9000]),t="l")
  # abline(h=thres[1:5])
  ##### Spike detection
  ########################
  ##########################
  #küszöb meghatározása
  thres<-numeric(cs)
  thres<-apply(adat.filt.mua,1,sd)*(-2)
  cat("Kuszob",thres)
  #thres<-rep(-170,cs)
  #################  SPIKEDETEKTÁLÁS   #####################
  cat('Spike detection')
  
  
  #ez az eltolás a kezdőpont plusz az x-szeri cucc miatt van
  plus<-(q-1)*dt*mintf+start*mintf
  
  
  #for(ch in 2:(cs-1)){
  for(sz in 2:(csathossz-1)){
    ch<-csatorna[sz]
    wow<-numeric() #mikor átlépi a küszöböt
    
    felablak<-ablak/2*mintf
    
    for(i in felablak:(dt*mintf-felablak)){
      if(adat.filt.mua[ch,i]<=thres[ch] && adat.filt.mua[ch,i-1]>thres[ch]) 
        wow<-c(wow,i+plus)
      
    }
    if(length(wow)!=0){
      #spike-ok kiírása
      for(l in 1:length(wow)){
        a<-adat.filt.mua[(ch-1):(ch+1),(wow[l]-plus-elott):(wow[l]-plus+utan)]
        
        if(q==1 && l==1) spikes1[[ch]]<-a
        if (q!=1 || l!=1) spikes1[[ch]]<-matrix(c(spikes1[[ch]],a),nrow=3)
      }
      wowtime1[[ch]]<-c(wowtime1[[ch]],wow/mintf)
    } #if 
    
    wowtime[[ch]]<-wowtime1[[ch]]
    spikes[[ch]]<-spikes1[[ch]]
    
    
    cat(ch)
  }#ch
  
  #plot(adat[11,(wow[12]-felablak+1):(wow[12]+felablak)],t='l')
  
} #q

##############
## Setting working directory for writing out data
 setwd(mentes)


####################x Up and down filtering
####### 
# up & down state detektálás
detectState<-"yes" #"no" #"yes"
if (detectState=="yes"){

  source(paste(forras1,"UpDownDetect.R",sep=""))
#   Gamma RMS 50 egysgnyi mozgóátlag
#melyik csatorna cuccait ábrázoljuk??
#csat<-8


rmsszamol<-function(adatsor){
  #hossz<-50*adatsec #hány ms-ra simítunk
  hossz<-50*adatsec
  rms<-sqrt( stats::filter(adatsor,rep(1/hossz,hossz), sides=2))
  return(rms)
}


#hist(adat.rms[1,],breaks=100)
#hist(adat.rms[2,],breaks=100)
#plot(adat.rms[1,],t='l')
#function for detecting up or down state


adat.gamma.rms<-t(apply(adat.filt.gamma^2,1,rmsszamol))
adat.mua.rms<-t(apply(adat.filt.mua^2,1,rmsszamol))
#Calculate the mean for the channels in the certain areas.
#adat.gamma.rms.sum.s<-colMeans(adat.gamma.rms[1:16,])
#adat.gamma.rms.sum.v<-colMeans(adat.gamma.rms[17:32,])
#adat.mua.rms.sum.s<-colMeans(adat.mua.rms[1:16,])
#adat.mua.rms.sum.v<-colMeans(adat.mua.rms[17:32,])
WhichS<-which.max(apply(adat.mua.rms,1,sd,na.rm=TRUE)[1:16])
WhichV<-which.max(apply(adat.mua.rms,1,sd,na.rm=TRUE)[17:32])+16

#For the sensory motor cortex
#felMua.ido,leMua.ido,felGamma.ido,leGamma.ido
SUD<-UpDownDetect(adat.mua.rms[WhichS,], adat.gamma.rms[WhichS,])
#For the visual cortex
#VUD<-UpDownDetect(adat.mua.rms.sum.v,adat.gamma.rms.sum.v)
VUD<-UpDownDetect(adat.mua.rms[WhichV,], adat.gamma.rms[WhichV,])

nev<-"SensoryUpMua"
write.table(SUD$felMua.ido, nev)
nev<-"SensoryDownMua"
write.table(SUD$leMua.ido, nev)
nev<-"SensoryUpGamma"
write.table(SUD$felGamma.ido, nev)
nev<-"SensoryDownGamma"
write.table(SUD$leGamma.ido, nev)

nev<-"VisualUpMua"
write.table(VUD$felMua.ido, nev)
nev<-"VisualDownMua"
write.table(VUD$leMua.ido, nev)
nev<-"VisualUpGamma"
write.table(VUD$felGamma.ido, nev)
nev<-"VisualDownGamma"
write.table(VUD$leGamma.ido, nev)

#Lets check on each thalamical and cortical channel, thet when is up and when is down state...
#this just works, if we start with down state
lengthVup<-min(length(VUD$felMua.ido),length(VUD$leMua.ido))
lengthSup<-min(length(SUD$felMua.ido),length(SUD$leMua.ido))
UpDownStatesS<-vector("list",cs)
UpDownStatesV<-vector("list",cs)

for (csat in 1:cs){
  if (csat==33) next
  SUP<-numeric()
    for(sup in 1:lengthSup){
      SUP<-c(SUP, intersect(which(wowtime1[[csat]] <SUD$leMua.ido[sup] ), which(wowtime1[[csat]]  > SUD$felMua.ido[sup] )))
      
    }
  
  VUP<-numeric()
  for(vup in 1:lengthVup){
    VUP<-c(VUP, intersect(which(wowtime1[[csat]] <VUD$leMua.ido[vup] ), which(wowtime1[[csat]]  > VUD$felMua.ido[vup] )))
    
  }
  
  statedeters<-rep(0,length(wowtime1[[csat]]))
  statedeters[SUP]<-1
  UpDownStatesS[[csat]]<-statedeters
  
  statedeterv<-rep(0,length(wowtime1[[csat]]))
  statedeterv[VUP]<-1
  UpDownStatesV[[csat]]<-statedeterv
  
}


cat("Calculation of Up and Down States - Overall.... Ready!!!","\n")


#########################
ForAllChannels<-'No'
if(ForAllChannels=='Yes'){
  updown.list<-vector("list",32)
  updown.mua.list<-vector("list",32)
  for(csat in 1:32){
    cat(csat)
    
    allapot<-c(rep(0,dt*mintf))
    allapot.mua<-c(rep(0,dt*mintf))
    
    hol<-detekt(adat.gamma.rms[csat,])
    hol.mua<-detekt(adat.mua.rms[csat,])
    allapot[hol]<-1
    allapot.mua[hol.mua]<-1
    #Which spike is in up state
    updown<-c(rep(0,length(wowtime1[[csat]])))
    updown.mua<-c(rep(0,length(wowtime1[[csat]])))
    updown[which(is.element(wowtime1[[csat]]*mintf,hol)==TRUE)]<-1
    updown.mua[which(is.element(wowtime1[[csat]]*mintf,hol.mua)==TRUE)]<-1
    
    updown.list[[csat]]<-updown
    updown.mua.list[[csat]]<-updown.mua
    #mitCsat<-17
    #plot(adat.filt.gamma[mitCsat,1:100000],t='l')
    #lines(adat.gamma.rms[mitCsat,1:100000],col='BLUE')
    #lines(allapot[1:100000]*500,col='RED')
    
    #plot(adat.filt.mua[mitCsat,1:10000],t='l')
    #lines(adat.mua.rms[mitCsat,1:10000],col='BLUE')
    #lines(allapot.mua[1:10000]*200,col='GREEN')
    #ez csak az adorr csatornára vonatkozik
    fel<-numeric()
    le<-numeric()
    for(i in 2:(dt*mintf)){
      if(allapot.mua[i-1]==0 && allapot.mua[i]==1){
        fel<-c(fel,i+start*mintf)
      }
      if(allapot.mua[i-1]==1 && allapot.mua[i]==0){
        le<-c(le,i+start*mintf)
      }
    }
    
    #throwing out the detected changes in up and down states, whoch are shorter, than 50 ms
    torol<-numeric()
    for(i in 1:length(fel)){
      if(abs(le[i]-fel[i])< (50*adatsec))
        torol<-c(torol,i)
    }
    fel<-fel[-torol]
    le<-le[-torol]
    fel.ido<-fel/mintf
    le.ido<-le/mintf
    
    
    
    
    
    felnev<-paste('UPglobido_csat_',csat,sep='')
    write.table(fel.ido, felnev)
    lenev<-paste('DOWNglobido_csat_',csat,sep='')
    write.table(le.ido, lenev)
    
    #fel állapot detektálása
    #fel<-vector("list",32)
    #le<-vector("list",32)
    #for(i in 2:(dt*mintf)){
    #if(allapot.mua[i-1]==0 && allapot.mua[i]==1){
    #fel[[csat]]<-c(fel[[csat]],i+start*mintf)
    #}
    #if(allapot.mua[i-1]==1 && allapot.mua[i]==0){
    #le[[csat]]<-c(le[[csat]],i+start*mintf)
    #}
    #}
    #az elso ár utolsó hamis detekció...
    
    
    #hol<-apply(adat.gamma.rms,1,detekt)
    #hol2<-as.matrix(hol)
    #allapot[hol2]<-1
    y <- adat[csat,]
    z <- adat.filt.gamma[csat,]
    v<-adat.filt.mua[csat,]
    w<-adat.gamma.rms[csat,]
    wmua<-adat.mua.rms[csat,]
    
    
    #source('SlidingPlot.R')
    
    
    abranev<-paste('UD_csat_',csat,'.png',sep='')
    #abranev<-paste('/media/BA0ED4600ED416EB/agy/sCSD_git/leiras/pics/UD2_csat_',csat,'.png',sep='')
    
    png(abranev, width=1600, height=800,pointsize = 30)
    par(mfrow=c(4,1), mar=c(1.5,1,0.5,0.5) ,oma=c(0.5,1,2,0.5) )
    cim<-paste(csat,'. csatorna',sep='')
    
    layout(matrix(c(1:4),4,1),widths=c(1,1,1,1),   	heights=c(2,2,2,1))
    x<-1:(dt*mintf)
    plot(x/adatsec,y[x],ylim=range(y),t='l', xaxt="n")
    legend("bottomright","Nyers adat",pch=20,bg="WHITE")
    plot(x/adatsec,z[x],t='l', col='RED', xaxt="n")
    lines(x/adatsec,w[x], col='PURPLE',lwd=2)
    legend("bottomright",c("gamma", "gamma RMS"),pch=20,col=c('RED','PURPLE'),bg="WHITE")
    plot(x/adatsec,v[x],t='l', col='BLUE', xaxt="n")
    lines(x/adatsec,wmua[x], col='PURPLE',lwd=2)
    legend("bottomright",c("MUA","MUA RMS"),pch=20,col=c('BLUE','PURPLE'),bg="WHITE")
    plot(x/adatsec,allapot[x],t='l',xlab='ms',col='RED',lwd=2)
    lines(x/adatsec,allapot.mua[x],col='BLUE',lwd=2)
    legend("bottomright",c("gamma","MUA"),col=c('RED','BLUE'),pch=20,bg="WHITE")
    mtext(cim, NORTH<-3, line=0, adj=0.5, cex=1, outer=TRUE)
    dev.off()
  }
  
}

} #up & down
####################  PCA   ###########################
cat('PCA')


library(mclust)

newDim<-6
MIMI<-vector("list",cs)
kod1<-vector("list",cs)
kod2<-vector("list",cs)
kod<-vector("list",cs)
klaszt.jell<-vector("list",cs)
#for(ch in (2:(cs-1))){
#for(ch in 2:10){
for(sz in 2:(csathossz-1)){
ch<-csatorna[sz]

al<-numeric()
af<-numeric()
valami<-numeric()
klaszter<-numeric()

klaszt.uncertainty<-numeric()
klaszt.bic<-numeric()
mimi<-numeric()
MIMI<-list(ch=numeric())
if((hossz*3)<length(wowtime[[ch]])){        #van-e elég adat a pca-hoz hány csatornára klaszterezünk*adatpont  

#}
#spikes<-matrix(spikes,nrow=ablak*mintf)   
tuskek<-matrix(spikes[[ch]],nrow=hossz*3)
tuskek<-tuskek[c(seq(1,hossz*3,3),seq(2,hossz*3,3),seq(3,hossz*3,3)),]
tuskek<-t(tuskek)


tuskek.pca<-princomp(tuskek)
#plot(tuskek.pca$scores[1,],t='l')

 #ez mennyire fontos lépés?  

valami<-tuskek%*%tuskek.pca$loadings[,1:newDim] #spm alapján  
#pairs(valami)

klaszter<-Mclust(valami, minG=2,maxG=15)
klaszterplotname<-paste("klaszPlot_csat_",ch,".png", sep = "")
#png(pointsize=bm,filename = klaszterplotname, width = 900, height = 900)
#par(mfrow=c(2,2))
#plot(klaszter,ask=FALSE)
#dev.off()
klaszt.uncertainty<-klaszter$uncertainty
klaszt.bic<-klaszter$BIC
bicname<-paste("bic",ch,".png", sep = "")
png(bicname)
matplot(klaszter$BIC[,1:10], pch = 1:10, type = "o", col = rainbow(10))
dev.off()


#‘cls.scatt.data’ returns an object of class ‘"list"’. Intracluster diameters: ‘intracls.complete’, ‘intracls.average’,     ‘intracls.centroid’, are stored in vectors and intercluster     distances: ‘intercls.single’, ‘intercls.complete’, ‘intercls.average’, ‘intercls.centroid’, ‘intercls.ave_to_cent’,     ‘intercls.hausdorff’ in symmetric matrices.  Vectors' lengths and     both dimensions of each matrix are equal to number of clusters.     Additionally in result list ‘cluster.center’ matrix (rows correspond to clusters centers) and ‘cluster.size’ vector is given     (information about size of each cluster).
klaszt.jell[[ch]]<-cls.scatt.data(valami,klaszter$classification)
#klasztjellname<-paste('klasztjell_ch_',ch,sep='')
klasztjellnamekpca<-paste('klasztjellkpca_ch_',ch,sep='')
lapply(klaszt.jell[[ch]], cat, "\n", file=klasztjellnamekpca, append=TRUE)
#klaszter
#a valami hordozza a klaszterezendő adatpontokat, a
#klaszter$classification pedig hogy melyik klaszterbe tartoznak
#távolságszámolás
#cls.scatt.data
#klaszt.jell<-cls.scatt.data(valami,klaszter$classification)
#%klasztjellname<-paste('klasztjell_ch_',ch,sep='')
#write.table(klaszt.jell,klasztjellname) #??
if (ch<33){
  mimi<-c(klaszter$classification,wowtime[[ch]],klaszt.uncertainty,UpDownStatesS[[ch]],UpDownStatesV[[ch]]) #,updown.list[[ch]],updown.mua.list[[ch]])
  dimMimi<-length(klaszter$classification)
  #klaszterszámok és pontszám egymás mellett    
  mimi<-matrix(mimi, nrow=dimMimi, dimnames = list(c(1:dimMimi),
                                                          c("klaszterszam", "globido","uncertainty","SUP","VUP"))) #,"updown","updown.mua")))
}
if (ch>33){
  dimMimi<-length(klaszter$classification)
  mimi<-c(klaszter$classification,wowtime[[ch]],klaszt.uncertainty,UpDownStatesS[[ch]],UpDownStatesV[[ch]])
  
  #klaszterszámok és pontszám egymás mellett    
  mimi<-matrix(mimi, nrow=dimMimi, dimnames = list(c(1:dimMimi),
                                                          c("klaszterszam", "globido","uncertainty", "SUP","VUP")))
}



colnames(valami)<-c('1. főkomponens','2. főkomponens','3. főkomponens','4. főkomponens','5. főkomponens','6. főkomponens')


if(klaszter$G!=1){
klasztername<-paste("klaszter_csat_",ch,".png", sep = "")
png(pointsize=bm,filename = klasztername, width = 700, height = 700)
#randProj(valami, parameters=klaszter$parameters,classification=klaszter$classification)
focim<-paste('A ',ch,'. csatornán detektált tüskékből álló klaszterek 2 dimenziós vetülete')

par(mfrow=c(1,1),oma = c(0, 0, 3, 0))
coordProj(valami, classification=klaszter$classification,dimens=c(1,2))
maxklaszter<-max(klaszter$classification)
nevklaszter<-c(rep(0,maxklaszter))
#szimbol<-c(mclustOptions()$classPlotSymbols[1:maxklaszter])
#szinek<-c(mclustOptions()$classPlotColors[1:maxklaszter])
#for(nevk in 1:maxklaszter) nevklaszter[nevk]<-paste(nevk,'. klaszter',sep='')
#legend("topright",c(nevklaszter), pch=szimbol,col=szinek ,bg='white')
#mtext(focim,outer = TRUE, cex = 1.2)
#clPairs(valami,mimi[,1],main=paste('A ',ch,'. csatornán detektált tüskékből álló klaszterek vetülete'),labels=c('1. főkomponens','2. főkomponens','3. főkomponens','4. főkomponens','5. főkomponens','6. főkomponens'))
dev.off()
}

}else mimi<-"nincs eleg spike a PCA-hoz"

#átlagszámolás
#sum<-numeric()
#db<-0
#for(i in i:length(wow)){
#if (mimi[i,1]==1) sum<-sum+adat[ch,(mimi[i,2]-felablak+1):(mimi[i,2]+felablak)]
MIMI[[ch]]<-mimi
cluname<-paste(fajlnev,".clu.",ch,sep='')
resname<-paste(fajlnev,".res.",ch,sep='')


if(MIMI[[ch]]!="nincs eleg spike a PCA-hoz"){

write.table(MIMI[[ch]][,1],
file=cluname,row.names=FALSE,col.names=FALSE)


write.table(MIMI[[ch]][,2]*mintf,
file=resname,row.names=FALSE,col.names=FALSE)

uncer.name<-paste(fajlnev,"_uncertaunty_",ch,sep='')
write.table(klaszt.uncertainty,
            file=uncer.name,row.names=FALSE,col.names=FALSE)
bic.name<-paste(fajlnev,"_bic_",ch,sep='')
write.table(klaszt.bic,
            file=bic.name,row.names=FALSE,col.names=FALSE)


} # elég spike
if(MIMI[[ch]]=="nincs eleg spike a PCA-hoz"){

write.table(0,
file=cluname,row.names=FALSE,col.names=FALSE)


write.table(0,
file=resname,row.names=FALSE,col.names=FALSE)
}#nincs elég spike

miminev<-paste('MIMI_ch',ch,sep='')
write.table(MIMI[[ch]],file=miminev)

}
return(paste("Spike Sorting ready","\n"))
}#SpikeSorting
