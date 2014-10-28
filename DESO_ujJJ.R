
csat.rend<-scan("chOrder32lin16lin16lin.txt")
#mik<-c("p3d6.6flash.dat","p2d6.6flash.dat","p2d6.6.dat")
#
## open the file for reading in binary mode
fid<-file(fajlnev,"rb")

#
#fontsize for figures
bm<-18 #betűméret az ábrákon

#melyik csatornákra vagyunk kiváncsiak?
## create a vector of length 65 for the channel position
csatorna<-c(1:65)

## number of channels
csathossz<-length(csatorna)

#rajzoljon-e sok cuccot?
## logical string variable for determining whether to print or not
rajzol='nem'
#cat('honnan olvastam be?
start<-1 #(sec-ben,10-snként változtatható,5-re végződő...) #

#mennyi időt olvasson be (sec)?
## time step 
dt<-10 #(s-ban,10-enként változtatható)
#how many times  
x<-1 #hányszor  
#sáváteresztő szűrő 
## bandpass filter lower and upper frequency limits
honnan<-300 #alső határ 
meddig<-3000 #felső határ 
## number of channels
cs<-65  #csatornák száma ## number of channels


#mekkora időablakkal nézzük (sec)? 
## window
ABLAK<-c(0.02,0.004)  #a klaszteratlagnal stb abrazolas idobeli hossza  ## window vector of length 2
ablak<-0.02 #ide a nagyobb időablak kell, utána majd a plotolásnál átíródik kisebbre
mintf<-20000 #mintavételezési frekvencia  ## sampling frequency
adatsec<-20




setwd(mentes)  ## set working directory
dirname<-paste(fajlnev,"_",start,'_','sec_est',sep='')  ## name a new directory
##dirname<-"Nov26c"
dir.create(dirname) #csinálunk egy mappát!!!  ## create the directory
setwd(dirname) 




#utan<-20
utan<-13
#elott<-9
elott<-6
hossz<-utan+1+elott

#agyi rétegek (sCSD rétegenként(mármint adottba tartozó sejtenként))  
#s1
s1<-c(1:16)  ## create vector for s1
#v1 sejtek
v1<-c(17:32) ## create vector for v1 

s2<-33 #üres
#thalamus sejtek
thal<-c(34:65)  ## create vector for thalamus



#################
wowtime<-vector("list",cs) ## create vector of length cs=65
spikes<-vector("list",cs)  ## create vector of length cs=65


#################SZŰRÉS############################
#szűrés paraméterei
setwd(parent)
## Make variables for entry numbers that correspond to frequencies at limits of preserved frequency range 
honnan1<-honnan*dt
meddig1<-meddig*dt
#szűrés logsztikus szigmoid függyvénnyel simítva
szorzo<-0.1

wowtime1<-vector("list",cs)
spikes1<-vector("list",cs)

a<-numeric()  ## creates numeric variable (vector)


for(q in 1:x){

adat<-numeric()
adat.filt<-numeric()

#if(((x-1)dt+start)== ) ##ltp időszaka  


seek(fid,where=((q-1)*cs*mintf*dt+start*cs*mintf)*2,origin="start",rw="read",)

#a spike a teljes időszak közepén van 

adat<-readBin(fid, what='integer', size=2, n=cs*mintf*dt,endian="little",signed="TRUE")
adat<-matrix(adat,nrow=cs)
adat<-adat[csat.rend,] #csatornák sorbarakása fizikai helyzetük alapján 
##adat.fft<-fft(adat)  ##Note: for a matrix, the fft command gives the multivariate transform, which takes the composition of the ffts of the rows and columns
adat.fft<-t(mvfft(t(adat)))  ## mvfft takes the ft of each column separately

##############################################
setwd(mentes)
setwd(dirname)
png(pointsize=bm,'nyersadat_cs8.png')
abr<-adat[8,]
plot(c(1:length(abr))/mintf,abr,xlab='idő [s]',ylab='potenciál [uV]',main='Nyers adat',t='l')
dev.off()

nyers1<-paste("ny_cs8_",start+(q-1)*dt, ".png",sep = "")
png(pointsize=bm,nyers1)
abr<-adat[8,]
plot(c(1:length(abr))/mintf+start+(q-1)*dt,abr,xlab='idő [s]',ylab='potenciál [uV]',main='Nyers adat',
t='l'
#,ylim=c(-1000,1000)
)
dev.off()


png(pointsize=bm,'nyersadat_cs8_rovid.png')
abr<-adat[8,1:(8*mintf)]
plot(c(1:length(abr))/mintf,abr,xlab='idő [s]',ylab='potenciál [uV]',main='Nyers adat',t='l')
dev.off()




png(pointsize=bm,'nyersadat.png')
abr<-adat[,1:3000]
plot(c(1:length(abr[1,]))/mintf,abr[1,],xlab='idő [s]',ylab='potenciál [uV]',main='Nyers adat',t='l',
ylim=c(-15*150,300))
for(j in 2:16) lines(c(1:length(abr[1,]))/mintf ,abr[j,]-(j-1)*150,t='l' )
dev.off()




png(pointsize=bm,'fftadat_cs8.png')
abr<-Mod(adat.fft[8,])
plot(c(1:length(abr))/dt,abr,xlab='frekvencia [Hz]',ylab='amplitúdó',main='Fourier spektrum',
ylim=c(0,2000000),t='l', xlim=c(1,10000)
)
dev.off()

#fourierből kivágás
#f1<-c(1:(honnan1+100))-honnan1
#f2<-c(1:(honnan1+100))-100
#f3<-c(meddig1:(dt*mintf/2)+100)-meddig1-100
#f4<-c((dt*mintf/2):(dt*mintf-meddig1+100))-(dt*mintf-meddig1)


#adat.fft[,1:(honnan1+100)]<-adat.fft[,1:(honnan1+100)]*1/(1+exp(szorzo*(-f1)))
#adat.fft[,(dt*mintf-honnan1-100):(dt*mintf)]<-adat.fft[,(dt*mintf-honnan1-100):(dt*mintf)]*1/(1+exp(szorzo*(f2)))
#adat.fft[,(meddig1-100):(dt*mintf/2)]<-adat.fft[,meddig1:(dt*mintf/2-100)]*1/(1+exp(szorzo*(f3)))
#adat.fft[,(dt*mintf/2):(dt*mintf-meddig1+100)]<-adat.fft[,(dt*mintf/2):(dt*mintf-meddig1+100)]*1/(1+exp(szorzo*(-f4)))

#kell valahol vágni????

#szűrés símítása tanh függvénnyel  ## use gaussian for smoothing since ft(gauss)=gauss
#jó az az érték, amit [-1;1] között felvesz pl 100 pontban
#100 ##1000 ponton keresztül símítunk
#alsó frekinél 100 Hz-es tartományban legyen a szűrés
p1<-100*dt
##simit1<-seq(-3,3,6/p1)  ## Create a sequence of numbers 
simit1<-seq(-10,0,10/p1)  ## Create a sequence of numbers 
sigma<-2.5   ## standard deviation for gaussian

#adat.fft[,1:honnan1]<-0
adat.fft[,1:(honnan1-p1/2)]<-0
## adat.fft[,(honnan1-p1/2):(honnan1+p1/2)]<-adat.fft[,(honnan1-p1/2):(honnan1+p1/2)]*tanh(simit1)
### adat.fft[,(honnan1-p1/2):(honnan1+p1/2)]<-adat.fft[,(honnan1-p1/2):(honnan1+p1/2)]*(.5+.5*tanh(simit1))
adat.fft[,(honnan1-p1/2):(honnan1+p1/2)]<-adat.fft[,(honnan1-p1/2):(honnan1+p1/2)]*(exp(-((simit1)^2)/(2*sigma^2)))

#adat.fft[,(dt*mintf-honnan1):(dt*mintf)]<-0
adat.fft[,(dt*mintf-honnan1+p1/2):(dt*mintf)]<-0
## adat.fft[,(dt*mintf-honnan1-p1/2):(dt*mintf-honnan1+p1/2)]<-adat.fft[,(dt*mintf-honnan1-p1/2):(dt*mintf-honnan1+p1/2)]*rev(tanh(simit1))
### adat.fft[,(dt*mintf-honnan1-p1/2):(dt*mintf-honnan1+p1/2)]<-adat.fft[,(dt*mintf-honnan1-p1/2):(dt*mintf-honnan1+p1/2)]*rev(.5+.5*tanh(simit1))
adat.fft[,(dt*mintf-honnan1-p1/2):(dt*mintf-honnan1+p1/2)]<-adat.fft[,(dt*mintf-honnan1-p1/2):(dt*mintf-honnan1+p1/2)]*rev(exp(-((simit1)^2)/(2*sigma^2)))

#alsó frekinél 1000 Hz-es tartományban legyen a szűrés
p2<-1000*dt
## simit2<-seq(-3,3,6/p2)  
simit2<-seq(-10,0,10/p2)

#adat.fft[,meddig1:(dt*mintf-meddig1)]<-0
adat.fft[,(meddig1+p2/2):(dt*mintf-meddig1-p2/2)]<-0
## adat.fft[,(meddig1-p2/2):(meddig1+p2/2)]<-adat.fft[,(meddig1-p2/2):(meddig1+p2/2)]*rev(tanh(simit2))
### adat.fft[,(meddig1-p2/2):(meddig1+p2/2)]<-adat.fft[,(meddig1-p2/2):(meddig1+p2/2)]*rev(.5+.5*tanh(simit2))
adat.fft[,(meddig1-p2/2):(meddig1+p2/2)]<-adat.fft[,(meddig1-p2/2):(meddig1+p2/2)]*rev(exp(-((simit2)^2)/(2*sigma^2)))

## adat.fft[,(dt*mintf-meddig1-p2/2):(dt*mintf-meddig1+p2/2)]<-adat.fft[,(dt*mintf-meddig1-p2/2):(dt*mintf-meddig1+p2/2)]*tanh(simit2)
###adat.fft[,(dt*mintf-meddig1-p2/2):(dt*mintf-meddig1+p2/2)]<-adat.fft[,(dt*mintf-meddig1-p2/2):(dt*mintf-meddig1+p2/2)]*(.5+.5*tanh(simit2))
adat.fft[,(dt*mintf-meddig1-p2/2):(dt*mintf-meddig1+p2/2)]<-adat.fft[,(dt*mintf-meddig1-p2/2):(dt*mintf-meddig1+p2/2)]*(exp(-((simit2)^2)/(2*sigma^2)))



png(pointsize=bm,'fftszurtadat_cs8.png')
abr<-Mod(adat.fft[8,])
plot(c(1:length(abr))/dt,abr,xlab='frekvencia [Hz]',ylab='amplitúdó',main='Fourier spektrum',
ylim=c(0,2000000),t='l',xlim=c(1,10000))
dev.off()

## fft uses multivariate transform, composing fts of rows and columns
##adat.ifft<-fft(adat.fft,inverse=TRUE)/(length(adat.fft[1,]))/cs 
## We want fft of each row so we use mvfft takes ft of each column of matrix, which is what we want
adat.ifft<-t(mvfft(t(adat.fft),inverse=TRUE))/(length(adat.fft[1,]))  

adat.filt<-Re(adat.ifft)

png(pointsize=bm,'szurtadat_cs8.png')
abr<-adat.filt[8,]
plot(c(1:length(abr))/mintf,abr,xlab='idő [s]',ylab='potenciál',main='Szűrt adat',t='l')
dev.off()

png(pointsize=bm,"szuretlen_szurt_cs8.png")
par(mfrow=c(2,1) )
abr<-adat[8,]
plot(c(1:length(abr))/mintf,abr,xlab='idő [s]',ylab='potenciál',main='Szűretlen adat',t='l',xlim=c(5,6),ylim=c(-500,400))
abr<-adat.filt[8,]
plot(c(1:length(abr))/mintf,abr,xlab='idő [s]',ylab='potenciál',main='Szűrt adat',t='l',xlim=c(5,6),ylim=c(-500,400))
dev.off()


rm(adat.fft,adat.ifft)


#################x ez egy érdekes rész, érdemes a többire is kirajzolni
rajzol<-0
if(rajzol==1){
png(pointsize=bm,"Szűrt adat (1-15. csatorna)",width = 1000, height = 700)
plot(adat.filt[1,290000:292000],t="l", ylim=c(-5000,300),main="Szűrt adat (1-15. csatorna)")
for(i in 1: 14) lines(adat.filt[i+1,290000:292000]-i*300)
dev.off()

png(pointsize=bm,"Szűrt adat (16-32. csatorna)",width = 1000, height = 700)
plot(adat.filt[16,290000:292000],t="l", ylim=c(-5000,300),main="Szűrt adat (16-32. csatorna)")
for(i in 1: 16) lines(adat.filt[i+16,290000:292000]-i*300)
dev.off()

png(pointsize=bm,"Szűrt adat (34-65. csatorna)",width = 1000, height = 700)
plot(adat.filt[34,290000:292000],t="l", ylim=c(-10000,300),main="Szűrt adat (34-65. csatorna)")
for(i in 1: 31) lines(adat.filt[i+34,290000:292000]-i*300)
dev.off()


png(pointsize=bm,"Szűrt adat (1-65. csatorna)",width = 1000, height = 700)
plot(adat.filt[1,290000:292000],t="l", ylim=c(-20000,300),main="Szűrt adat (1-65. csatorna)",col="ORANGE", xlab="100 ms")
for(i in 1: 15) lines(adat.filt[i+1,290000:292000]-i*300,col="ORANGE")
for(i in 1: 16) lines(adat.filt[i+16,290000:292000]-(i+14)*300,col="RED")
for(i in 1: 33) lines(adat.filt[i+32,290000:292000]-(i+31)*300, col="PURPLE")
dev.off()
#honnan ábrázolja
} #rajzol

hatar<-20000:30000
sokcsat.nev<-paste("Szűrt adat ",hatar[1],"-től",sep="")
png(pointsize=bm,sokcsat.nev,width = 1000, height = 700)
plot(adat.filt[1,hatar],t="l", ylim=c(-20000,300),main="Szűrt adat (1-65. csatorna)",col="ORANGE", xlab="500 ms")
for(i in 1: 15) lines(adat.filt[i+1,hatar]-i*300,col="ORANGE")
for(i in 1: 16) lines(adat.filt[i+16,hatar]-(i+14)*300,col="RED")
for(i in 1: 33) lines(adat.filt[i+32,hatar]-(i+31)*300, col="PURPLE")
dev.off()


##################################
############################
#ÁTRÁS
########################
##########################
#küszöb meghatározása
thres<-numeric(cs)
#thres<-apply(adat.filt,1,sd)*2
thres<-rep(-170,cs)
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
if(adat.filt[ch,i]<=thres[ch] && adat.filt[ch,i-1]>thres[ch]) 
wow<-c(wow,i+plus)

}
if(length(wow)!=0){
#spike-ok kiírása
for(l in 1:length(wow)){
a<-adat.filt[(ch-1):(ch+1),(wow[l]-plus-elott):(wow[l]-plus+utan)]

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


####################  PCA   ###########################
cat('PCA')


library(mclust)

newDim<-6
MIMI<-vector("list",cs)
kod1<-vector("list",cs)
kod2<-vector("list",cs)
kod<-vector("list",cs)
#for(ch in (2:(cs-1))){
#for(ch in 2:10){
for(sz in 2:(csathossz-1)){
ch<-csatorna[sz]

al<-numeric()
af<-numeric()
valami<-numeric()
klaszter<-numeric()
klaszt.jell<-numeric()
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
klaszt.uncertainty<-klaszter$uncertainty
klaszt.bic<-klaszter$BIC
#klaszter
#a valami hordozza a klaszterezendő adatpontokat, a
#klaszter$classification pedig hogy melyik klaszterbe tartoznak
#távolságszámolás
#cls.scatt.data
#klaszt.jell<-cls.scatt.data(valami,klaszter$classification)
#%klasztjellname<-paste('klasztjell_ch_',ch,sep='')
#write.table(klaszt.jell,klasztjellname) #??

mimi<-c(klaszter$classification,wowtime[[ch]])
#klaszterszámok és pontszám egymás mellett    
mimi<-matrix(mimi, nrow=length(mimi)/2, dimnames = list(c(1:(length(mimi)/2)),
           c("klaszterszam", "globido")))

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

#} #megy

