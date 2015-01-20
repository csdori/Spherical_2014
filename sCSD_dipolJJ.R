#itt már egy gömbmetszet mentén látszanak az áramforrássűrűség eloszlások
#csak a nem túl nayg csatornaszámú gömbös részekre jó, mert az utolsó 9 csatorna le van vágva
#source('/media/BA0ED4600ED416EB/agy/sCSD/sCSD_dipol.R')
#source('~/Documents/Desktop/KFKI/Thalamus/sCSD_git/sCSD_dipolJJ.R')  ## copy at R prompt to run on Desktop
#source('/home/jalics/sCSD_git/sCSD_dipolJJ.R')  ## copy at R prompt to run on tauri or uranus


#adatbeolvasás, paraméterek
#source("beolvasas_para.R")

if (DoClustering=='Yes'){
  cat("Clustering \n")
  Currentfolder<-getwd()
  setwd(parent)
  source(paste(forras1, 'filtering.R',sep=''),local=TRUE)
  SpikeSorting(fajlnev)
  #source(paste(forras1, 'DESO_ujJJ.R',sep=''))
  setwd(Currentfolder)
  rm(Currentfolder)
}

#### break here
#break 
setwd(forras1)

#library('clv')
library(class)
##library(fields)  ##NOTE: I don't have this package installed on Desktop. Do I need it???
##parent<-"~/Documents/Desktop/KFKI/Thalamus/sil20_erdekes"  ## On Desktop

start<-1

dirName<-paste("FS",FirstSpikeOnly,"_UpD",state.wanted,"_",start,"_","sec_est",sep='')

mappa<-paste(mentes,'/',dirName,sep='')  ## Folder name used in tuskerajz.R 
dir.create(mappa)

#interpoláció
#source("interpolacio.R")  ### We are using the interpolation function (inter) from the file dipolgombJJ.R

## since fields package not working causes error in image.plot

source("findpeaks.R")
source("imaging.R")
#source("LFPmean.R")
#nem lineárás színskálás ábrák
#source("nemlinplot.R")
source("nemlinplot2.R")       ### USING NEW FILE FOR PLOTTING WITH NONLINEAR COLOR SCALING
#for calculating cluster average
source("tuskerajz.R")
#dipolgombos
source("dipolgombJJ.R")   ## Using my revised code
#source("dipolgomb.R")
#hova mentsük a dolgokat?
source("coherence.R")






setwd(parent)
fid<-file(fajlnev,'rb')
#dirname1<-paste('dipol1_',fajlnev,'_',start,'_','sec_est',sep='')
#dir.create(dirname1) #csinálunk egy mappát!!! 
#setwd(dirname1) 


cat("Parameterek beolvasva \n")


##Calculation of coherence
if(CalcCoh=="yes"){

cat("Calculation of coherence")
q<-1
dts<-10 #data length to read in  s

seek(fid,where=((q-1)*cs*mintf*dts+start*cs*mintf)*2,origin="start",rw="read",)
adat<-readBin(fid, what='integer', size=2, n=cs*mintf*dts,endian="little",signed="TRUE")
adat<-matrix(adat,nrow=cs)
adat<-adat[csat.rend,]
#adat[1:16,]<-adat[1:16,]-colMeans(adat[1:16,])
#adat[17:32,]<-adat[17:32,]-colMeans(adat[17:32,])
#adat[34:65,]<-adat[34:65,]-colMeans(adat[34:65,])

FreqsLow<-c(1,5,7,20,50,200,500)
FreqsHigh<-c(5,7,20,50,200,500,1500)
Freqnb<-length(FreqsLow)
coherenceMatrix<-array(0,c(65,65,Freqnb))
phaseMatrix<-array(0,c(65,65,Freqnb))

for(cs1 in 1:65){
  cat("Koh cs1", cs1,"\n")

    for (cs2 in cs1:65){
    if (cs1==33 | cs2==33) next
	CohResult<-coherence(adat[cs1,],adat[cs2,],FreqsLow,FreqsHigh)
    coherenceMatrix[cs1,cs2,]<-CohResult[,3]
    phaseMatrix[cs1,cs2,]<-CohResult[,4]
    }
}

setwd(mentes)
for(fs in 1:Freqnb){
coherenceMatrix[,,fs]<-coherenceMatrix[,,fs]+t(coherenceMatrix[,,fs])
diag(coherenceMatrix[,,fs])<-1
phaseMatrix[,,fs]<-phaseMatrix[,,fs]-t(phaseMatrix[,,fs])
cohName<-paste("coh_",FreqsLow[fs],"_",FreqsHigh[fs],sep="")
phaseName<-paste("phase_",FreqsLow[fs],"_",FreqsHigh[fs],sep="")
write.table(coherenceMatrix[,,fs],cohName,col.names=FALSE,row.names=FALSE) 
write.table(phaseMatrix[,,fs],phaseName,col.names=FALSE,row.names=FALSE)
cohphaseName<-paste("cohphase_",FreqsLow[fs],"_",FreqsHigh[fs],"plot.png",sep="")
png(cohphaseName,width=1000,height=500)
par(mfrow=c(1,2))
#-function(y.skala,mit,   xfel, yfel,mitmain){
nemlinplot(c(1:65),coherenceMatrix[,,fs],"Channels","Channels",paste("Coherence ",FreqsLow[fs]," ",FreqsHigh[fs]," Hz",sep="" ))
nemlinplot(c(1:65),phaseMatrix[,,fs],"Channels","Channels", paste("Coherence ",FreqsLow[fs]," ",FreqsHigh[fs]," Hz",sep="" ))
dev.off()
}
#remove(c(adat,coherenceMatrix,phaseMatrix,cohName,phaseName,cohphaseName,fs,dts,FreqsLow, FreqsHigh,Freqnb,cs1,cs2))
}
##################
#mértékek számolása a különböző esetekben

#source("mertekek.R")

##########################################
############################################

#ide kéne az, hogy a program beolvassa a clu és nemtommilyen fájlokül az időpontokat és klaszterszámokat


##idoskal<-2 #itt alapból for ciklus van.. de hát... 
idoskal<-1   ##choose 4ms or 20ms timewindow   NOTE:SEE tuskerajz.R,beolvasas_para.R
ablak<-ABLAK[idoskal]   #az ábrák, számolások időablakának hossza
felablak<-ablak/2*mintf #adatpontokban az időablak hosszának fele

setwd(parent)

#setwd(dirname)

#melyik csatornákra vagyunk kiváncsiak?


#dirname1<-paste(ablak,'sec',sep='')  
#dir.create(dirname1) #csinálunk egy mappát!!! 
#setwd(dirname1) 


#for(ch in 2:(cs-1)){  #végigmegyünk a csatornákon
  for(sz in 1:csathossz){
ch<-csatorna[sz]
#kikörések: 33-ason nincs semmi, a 34-esre meg meg kell csinálni, hogy az
#alattalevőket nézi, mert most minden a felettelevőből számol
#cluname<-paste(parent,"/",fajlnev2,".clu.",ch,sep='')  ## using Acsadi's clustering data
cluname<-paste(mentes,"/",fajlnev,".clu.",ch,sep='')  ## using data from DESO_uj.R 
#resname<-paste(parent,"/",fajlnev2,".res.",ch,sep='')  ## using Acsadi's clustering data
resname<-paste(mentes,"/",fajlnev,".res.",ch,sep='')  ## using data from DESO_uj.R
#Cheking whether files exist
if(file.exists(cluname)==FALSE) next


klasz<-read.table(cluname) #a tüskék klaszterszámai
klaszter<-klasz[,1]
wo<-read.table(resname)
wow<-wo[,1]

if(ch>=min(s1) && ch<=max(s1)) {jel<-length(s1)
				a<-min(s1)
				b<-max(s1)
				sejt<-"s1"
				elektav<-100
				}
if(ch>=min(v1) && ch<=max(v1)) {jel<-length(v1)
				a<-min(v1)
				b<-max(v1)
				sejt<-"v1"
				elektav<-100
				}
if(ch>=min(thal) && ch<=max(thal)) {jel<-length(thal)
				a<-min(thal)
				b<-max(thal)
				sejt<-"thal"
				elektav<-50
				}


if(ch!=33){

if(length(klaszter)>0){
  
SNR<-vector(mode='numeric', length=max(klaszter))
for(k in 1:max(klaszter)){  #megyünk az adott csatornén lévő klasztereken végig



#############

#kirajzolunk pár tüskét egy adott klaszterből
#ebben van az adatbeovasás is
setwd( forras1)  ## set working directory to read in R file

ClustAve<-ClusterAverage(FirstSpikeOnly,state.wanted)
#########################
#mt[order(mt[,1]),]
###############

db<-ClustAve$db
#####################
if(db!=0 && db!=1){
ATLAG<-ClustAve$ATLAG
DATLAG<-ClustAve$DATLAG
isi<-ClustAve$isi
#interpolálás az r.all esetre

#t-test

#amplitudo
#ampl<-mean(DATLAG[ch,1:20])-min(DATLAG[ch,])


#szorasnegyzet<-negyzet/db-DATLAG*DATLAG 

#szoras<-sqrt(abs(szorasnegyzet))


##########
#cat(paste( "jel/zaj arany \n")
#SNR[k]<-abs(ampl)/mean(szoras[1:20,(ch-1):(ch+1)])

#########
#source("potrajzcsat.R")

#isi
try(source(paste(forras1,"isi.R",sep="")))
#}

#sCSD


#transzfermátrix
#konst<-1  #valójában 1/(4*pi*epsilon*sigma)

## Traditional CSD method
ITRAD<-array(0,c(jel,2*felablak))
for(lap in a:b){
  if (lap==a) ITRAD[lap-a+1,]<-( DATLAG[lap+1,] - DATLAG[lap,])/elektav^2
  else if (lap==b) ITRAD[lap-a+1,]<-( DATLAG[lap-1,] - DATLAG[lap,])/elektav^2
  else ITRAD[lap-a+1,]<-(DATLAG[lap-1,] + DATLAG[lap+1,] - 2*DATLAG[lap,])/elektav^2
}
remove(lap)



sejttav<-numeric() #sejread.tavtek távolsága az elektródától ill amplitúdójuk?

DistCompDirac<-numeric()       ## Array for saving computed distances using dirac method
DistCompHej<-numeric()		## Array for saving computed distance using shell method
DistCompDirac3<-numeric()
DistCompHej3<-numeric()
DistCompDirac4<-numeric()
DistCompHej4<-numeric()
MaxPots<-numeric()
CellNames<-character()

dmin<-1
dmax<-200
dstepsize<-1
somawidth<-5














later<-0
if(later==0){
Distances<-numeric()  
S<-numeric()
Shej<-numeric()
S2<-numeric()
S3<-numeric()
S3hej<-numeric()
S4<-numeric()
S4hej <- numeric()

##dmin<-10
##dmax<-200
##dstepsize<-1

for(d in seq(dmin,dmax,dstepsize)){          ### Trial distances
  ##for(d in 1:200){
Distances<-c(Distances,d)

legen<-legendre(d,DATLAG)
#I<-legen$I.dirac
I <-legen$I.dirac
Ihej <- legen$I.hej

A0A1dirac<-legen$A0A1dirac
A0A1hej <- legen$A0A1hej
sugar<-legen$hejvas
imaxhol<-which(max(abs(I))==abs(I),arr.ind=TRUE)
##I<-I/abs(I[imaxhol])  ## This seems to help in practice but not sure why???
##Ihej <- Ihej/max(abs(Ihej))
dipolcsat<-legen$dipolcsat
minPot<-legen$minPot


if(d==1) cat('A ', ch, '-as csatornan a dipolcsat:',dipolcsat , '\n')

#maxI<-max(I[,(felablak):(felablak+utan/2)])
#sumI2<-sqrt(sum(I[-(dipolcsat),(felablak):(felablak+utan/2)]^2))
#meanI<-mean(I[-(dipolcsat),(felablak):(felablak+utan/2)])
#s<-(maxI+meanI)/sumI2 #csúcsosságon alapuló mérték ahol az áramok összege 0
#s<-(sum(abs(I[-(dipolcsat+1),(felablak):(felablak+1)]))-sum(abs(I[dipolcsat+1,(felablak):(felablak+1)])))/max(-I[dipolcsat+1,(felablak):(felablak+1)])




spikehol<-seq(along=I[dipolcsat,])[I[dipolcsat,] == min(I[dipolcsat,])][1]
spikeholhej<-seq(along=Ihej[dipolcsat,])[Ihej[dipolcsat,] == min(Ihej[dipolcsat,])][1]
maxI<--I[dipolcsat,spikehol]
maxIhej<--Ihej[dipolcsat,spikeholhej]
##maxI<-max(-I[,spikehol])
##maxIhej<-max(-Ihej[,spikehol])

##I <- I/maxI
##Ihej <- Ihej/maxIhej

scalefactor<-sugar[1]^2*A0A1dirac[1,spikehol]^2  ## first term without dipole
scalefactorhej<-((sugar[1]^3)/3)*A0A1hej[1,spikeholhej]^2  ## first term without dipole
##scalefactorhej<-((sugar[1]^2)/1)*A0A1hej[1,spikeholhej]^2  ## first term without dipole

for(j in 2: dipolcsat){
  scalefactor<-scalefactor+sugar[j]^2*(A0A1dirac[j,spikehol]^2+A0A1dirac[dipolcsat+j-1,spikehol]^2)
  scalefactorhej<-scalefactorhej+((sugar[j]^3-sugar[j-1]^3)/3)*(A0A1hej[j,spikeholhej]^2+A0A1hej[dipolcsat+j-1,spikeholhej]^2)
 ##  scalefactorhej<-scalefactorhej+(sugar[j]^2)*(A0A1hej[j,spikeholhej]^2+A0A1hej[dipolcsat+j-1,spikeholhej]^2)
}
scalefactor<-sqrt(scalefactor)

scalefactorhej<-sqrt(scalefactorhej)


##sumI2<-sqrt(sum(I[-(dipolcsat),spikehol]^2))
meanI<--mean(I[-(dipolcsat),spikehol])  ### Note:  Remove the middle point
##meanI<--mean(I[-(dipolcsat:(dipolcsat+1)) ,spikehol])   ### Note:  Remove the middle 2 points for CSD calculation in line with neuron
meanIhej<--mean(Ihej[-dipolcsat ,spikeholhej])
##meanIhej<--mean(Ihej[-(dipolcsat:(dipolcsat+1)) ,spikeholhej]) ### Note:  Remove the middle 2 points

##s<-(maxI+meanI)/sumI2
##s<-(maxI-meanI)/sumI2


s<-(sugar[1]*maxI-meanI)/scalefactor
shej<-((sugar[1]^(3/2))*maxIhej-meanIhej)/scalefactorhej
##shej<-(sugar[1]*(maxIhej)-(meanIhej))/scalefactorhej





S<-c(S,s)
Shej<-c(Shej,shej)


#s2<-sum(abs(fft(I[ch-a+1,]/sumI2))[2:(ablak*mintf/2)])

if(ch==a){s2<-abs(sum(I[dipolcsat+2,(felablak-elott):(felablak+utan/2)]))/abs(sum(I[dipolcsat+1,(felablak-elott):(felablak+utan/2)])) }
if(ch!=a) { s2<-abs(sum(I[dipolcsat,(felablak-elott):(felablak+utan/2)]))/abs(sum(I[dipolcsat+1,(felablak-elott):(felablak+utan/2)])) }

#if(ch==a){s2<-abs(sum(I[ch-a+2,(felablak-elott):(felablak)]))/abs(sum(I[ch-a+1,(felablak-elott):(felablak+utan/2)])) }
#if(ch!=a) { s2<-abs(sum(I[ch-a,(felablak-elott):(felablak)]))/abs(sum(I[ch-a+1,(felablak-elott):(felablak+utan/2)])) } 

S2<-c(S2,s2) #ft összegen alapuló érték

#########

##s3<-sd(c(I[-(dipolcsat+1),(felablak-elott):(felablak+utan)]))/max(I[,(felablak-elott):(felablak+utan)])
##s3<-sd(c(I[-((dipolcsat-1):(dipolcsat+1)),spikehol]))/maxI   ## Find std deviation with all terms except middle 3 terms
s3<-sd(c(I[-(dipolcsat),spikehol]))/(sugar[1]*abs(maxI))   ## Find std deviation with all terms except middle term
S3<-c(S3,s3)

s3hej<-sd(c(Ihej[-(dipolcsat),spikeholhej]))/((sugar[1]^2)*abs(maxIhej))   ## Find std deviation with all terms except middle term
S3hej<-c(S3hej,s3hej)
                                      
s4<-sd(c(A0A1dirac[(2:dipolcsat),spikehol]))/(sugar[1]*abs(maxI))
S4<-c(S4,s4)

s4hej<-sd(c(A0A1hej[(2:dipolcsat),spikeholhej]))/((sugar[1]^2)*abs(maxIhej))
S4hej<-c(S4hej,s4hej)

############idáig műxik :)


}#d

S<-S/max(S)
Shej<-Shej/max(Shej)

S3<-S3/max(S3)
S3hej<-S3hej/max(S3hej)

S4<-S4/max(S4)
S4hej<-S4hej/max(S4hej)

###NOTE: tav,tavhej,tav3, etc. give the index of the distance vector: the actual distance is Distances("")
tav<-seq(along=S)[S == max(S)][1] # a tav csak akkor ennyi, ha a d egyesével megy!
tavhej<-seq(along=Shej)[Shej == max(Shej)][1] # a tav csak akkor ennyi, ha a d egyesével megy!
##tavhej<-seq(along=Shej)[Shej == findpeaks(Shej)[[2]][1]][1]
tav2<-seq(along=S2)[S2 == min(S2)][1]
tav3<-seq(along=S3)[S3 == min(S3)][1]
tav3hej<-seq(along=S3hej)[S3hej == min(S3hej)][1]
tav4<-seq(along=S4)[S4 == min(S4)][1]
##tav4<-seq(along=S4)[S4 == findpeaks(S4)[[4]][1]][1]
tav4hej<-seq(along=S4hej)[S4hej == min(S4hej)][1]

DistCompDirac<-c(DistCompDirac,Distances[tav])
DistCompHej<-c(DistCompHej,Distances[tavhej])
DistCompDirac3<-c(DistCompDirac3,Distances[tav3])
DistCompHej3<-c(DistCompHej3,Distances[tav3hej])
DistCompDirac4<-c(DistCompDirac4,Distances[tav4])
DistCompHej4<-c(DistCompHej4,Distances[tav4hej])
MaxPots<- c(MaxPots,abs(minPot))

}#later
###################################################x
#összefoglalóábra########################x
#source("osszefoglaloabra.R")

##plot(S,t='l',main=tav)
##lines(S2,col='RED')
##lines(S3,col='BLUE')


##legen<-legendre(tav,DATLAG)
if(is.na(tav)==FALSE)legen<-legendre(Distances[tav],DATLAG)
if(is.na(tavhej)==FALSE) legenhej<-legendre(Distances[tavhej],DATLAG)
if(is.na(tav3)==FALSE) legen3<-legendre(Distances[tav3],DATLAG)
if(is.na(tav4)==FALSE) legen4<-legendre(Distances[tav4],DATLAG)

dipolcsat<-legen$dipolcsat
dipolcsathej<-legenhej$dipolcsat
dipolcsat3<-legen3$dipolcsat
ch2 <- legen$ch2
ch2hej <- legenhej$ch2
ch2dirac3<- legen3$ch2
I1<-legen$I.dirac
Ihej1<-legenhej$I.hej
I1dirac3<-legen3$I.dirac
A0A1dirac<-legen$A0A1dirac
A0A1hej <- legen$A0A1hej

aramname<-paste("A0A1dirac_csat",ch,"k",k,sep='')
write.table(A0A1dirac,aramname,col.names=FALSE,row.names=FALSE) 

aramname<-paste("A0A1hej_csat",ch,"k",k,sep='')
write.table(A0A1hej,aramname,col.names=FALSE,row.names=FALSE) 


imaxhol<-which(max(abs(I1))==abs(I1),arr.ind=TRUE)
imaxholhej<-which(max(abs(Ihej1))==abs(Ihej1),arr.ind=TRUE)
imaxhol3<-which(max(abs(I1dirac3))==abs(I1dirac3),arr.ind=TRUE)

I<-legen$I.dirac.ip #itt már a nagyfelbontásút választjuk ki
Ihej <- legenhej$I.hej.ip
IhejDir <- legen$I.hej.ip  ## for plotting Ihej using the distance computed by Dirac method
Idirac4<-legen4$I.dirac.ip
felbontas<-legen$felbontas
felbontashej<-legenhej$felbontas
felbontas4<-legen4$felbontas
el.poz<-legen$el.poz



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

aramnev1<-paste("I_d_csat",ch,"k",k,sep='')
write.table(-I,aramnev1,col.names=FALSE,row.names=FALSE) 

} #



#########olyan ébra, amin rajta van a tCSD, sCSD (elektródák hat. meg gömbhéj, tCSD azonos vastagságú,
#tCSD azonos térfogatú gömbhéjak)

#source("osszefoglaloabra_thal.R")

#}  #wow

#tavnev<-paste('MIMI_ch',ch,sep='')
#klaszttav<-matrix(c(ch,k,db,tav2),nrow=1,col.names=c('csatorna','klaszter','db','tavolsag'))
#write.table(,file=miminev)

snrname<-paste('snr_',ch,sep='')
write.table(SNR,snrname) 
} #klaszter


} #mimi



} #ch!=33
} #cs



#X11()
png(pointsize=bm,filename = "Distances", width = 1000, height = 800)
plot(DistCompDirac,t='p',ylim=c(dmin,dmax),xlab='neurons',ylab='computed distance')
#lines(DistCompHej,t='p',col='BROWN')
#lines(DistCompDirac3,t='p',col='BLUE')
#lines(DistCompHej3,t='p',col='TURQUOISE')
lines(DistCompDirac4,t='p',col='RED')
#lines(DistCompDirac4,t='p',col='MAGENTA')
#legend("topright",c("Sdirac","Shej","Sdirac stdCSD","Shej stdCSD","Sdirac stdA0","Shej stdA0"),text.col=c("BLACK","BROWN","BLUE","TURQUOISE","RED","MAGENTA"))
dev.off()
#X11()
png(pointsize=bm,filename = "Errors", width = 1000, height = 800)
plot(DistCompDirac-DistCompDirac4,t='p',xlab='neurons',ylab='Difference of computed distances')
dev.off()
X11()
plot(DistCompDirac,MaxPots,t='p',xlim=c(dmin,dmax),xlab='distances',ylab='Potential Amplitudes')
lines(DistCompDirac4,MaxPots,t='p',col='RED')

png(pointsize=bm,filename = "DistvsAmpl", width = 1000, height = 800)
plot(DistCompDirac,MaxPots,t='p',xlim=c(dmin,dmax),xlab='Computed Cell-Electrode Distance',ylab='Potential Amplitude',main='Potential Amplitude vs. Distance')
lines(DistCompDirac4,MaxPots,t='p',col='RED')
dev.off()

png(pointsize=bm,filename = "DistvsAmplDirac", width = 1000, height = 800)
plot(DistCompDirac,MaxPots,t='p',xlim=c(dmin,dmax),xlab='Computed Cell-Electrode Distance',ylab='Potential Amplitude',main='Potential Amplitude vs. Distance')
dev.off()

png(pointsize=bm,filename = "DistvsAmplDirac4", width = 1000, height = 800)
plot(DistCompDirac4,MaxPots,t='p',col='RED',xlim=c(dmin,dmax),xlab='Computed Cell-Electrode Distance',ylab='Potential Amplitude',main='Potential Amplitude vs. Distance')
dev.off()

write.table(CellNames,"CellNames",col.names=FALSE,row.names=FALSE)

DistancesMatrix<-matrix(c(DistCompDirac,DistCompHej,DistCompDirac3,DistCompHej3,DistCompDirac4,DistCompHej4,MaxPots),ncol=7)

write.table(DistancesMatrix,"Distances6methods",col.names=FALSE,row.names=FALSE)



#file.copy('/home/jalics/sCSD_git/sCSD_dipolJJ.R', paste(mentes,"/",dirname,"/",dirname1,sep=""))
#file.copy('/home/jalics/sCSD_git/dipolgombJJ.R', paste(mentes,"/",dirname,"/",dirname1,sep=""))
#file.copy('/home/jalics/sCSD_git/nemlinplot2.R', paste(mentes,"/",dirname,"/",dirname1,sep=""))
#file.copy('/home/jalics/sCSD_git/beolvasas_para.R', paste(mentes,"/",dirname,"/",dirname1,sep=""))

cat("Reached End of sCSD_dipolJJ.R \n")



