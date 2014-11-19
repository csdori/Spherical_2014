FirstSpikeOnly<- "yes" #no
WhichState<-"Up" #"Up" "Down" "Both"
#cat("Only first spike of bursts taken into account")
MiMiName<-paste(mentes,"/MIMI_ch",ch,sep='')
MiMi<-read.table(MiMiName)
start<-1
ABLAK<-c(0.02,0.004)
##idoskal<-2 #itt alapból for ciklus van.. de hát...
idoskal<-2 #itt alapból for ciklus van.. de hát...
ablak<-ABLAK[idoskal]   #az ábrák, számolások időablakának hossza
felablak<-ablak/2*mintf #adatpontokban az időablak hosszának fele

setwd(parent)
tavg<-20
#dirname<-paste(fajlnev,'_',start,'_','sec_est',sep='')
setwd(mappa)
#setwd(dirname)
dirname1<-paste(ablak,'sec',sep='')  
##dir.create(dirname1) #csinálunk egy mappát!!!
dir.create(dirname1,showWarnings = FALSE) #csinálunk egy mappát!!! 
setwd(dirname1) 

#tüskerajzolás, többitől független

bitmap(pointsize=bm,paste("csat",ch,"k",k,".png",sep=""))
tol<-A
ig<-B

for(l in tol:ig){   # szétválasztjuk az ltp előtti és utáni klasztereket

if(klaszter[l]==k ){ 


###beolvasás
seek(fid,where=(wow[l]*cs-(felablak+1)*cs)*2,origin="start",rw="read",)
adat<-readBin(fid, what='integer', size=2, n=(2*felablak)*cs,endian="little",signed="TRUE")
adat<-matrix(adat,nrow=cs)
if(max(adat[ch,])-min(adat[ch,])< 2000) {  #ne legyen ennél nagyobb jel rajta
##########
#idopontok
#only first spikes in the birst??


if (FirstSpikeOnly=="yes"  & l>1 ){
  
 if (((wow[l]-elozo)/adatsec) > 5 ) { #csak a klaszteren belül nézi
#if (((wow[l]-wow[l-1])/adatsec) > 5 ) { #az egész csatornán nézi

idopontok<-c(idopontok,wow[l]/mintf)
db<-db+1

if(db!=1) isi<-c(isi,(wow[l]-elozo)/adatsec)  #ISI
elozo<-wow[l]

adat<-adat[csat.rend,]  #helyes elektródasorrend
data<-adat[ch,]   	#ez mér kell?

atlag<-adat
sum<-sum+atlag   #potenciálképek összege
negyzet<-negyzet+(atlag-rowMeans(data.matrix(atlag)))*(atlag-rowMeans(data.matrix(atlag)))

##if(db==1){ plot(c(1:length(data)/adatsec),data-mean(data),t='l',ylim=c(-1000,500),xlab='idő', ylab='potenciál (uV)', main='Klaszterátlag')
if(db==2){ plot(c(1:length(data)/adatsec),data-mean(data),t='l',ylim=c(-1000,500),xlab='ido', ylab='potenciál (uV)', main='Klaszterátlag')
#lines(c(1:length(data)/adatsec,al-mean(al),col='BLUE')
#lines(c(1:(felablak*2)),af-mean(af),col='RED')
}

if(db!=1 && db<100){lines(c(1:length(data)/adatsec),data-mean(data))
#lines(c(1:(felablak*2)),al-mean(al),col='BLUE')
#lines(c(1:(felablak*2)),af-mean(af),col='RED')
}


}
}
if (FirstSpikeOnly!="yes"){

idopontok<-c(idopontok,wow[l]/mintf)
db<-db+1

if(db!=1) isi<-c(isi,(wow[l]-elozo)/adatsec)  #ISI
elozo<-wow[l]

adat<-adat[csat.rend,]  #helyes elektródasorrend
data<-adat[ch,]   	#ez mér kell?

atlag<-adat
sum<-sum+atlag   #potenciálképek összege
negyzet<-negyzet+(atlag-rowMeans(data.matrix(atlag)))*(atlag-rowMeans(data.matrix(atlag)))

##if(db==1){ plot(c(1:length(data)/adatsec),data-mean(data),t='l',ylim=c(-1000,500),xlab='idő', ylab='potenciál (uV)', main='Klaszterátlag')
if(db==2){ plot(c(1:length(data)/adatsec),data-mean(data),t='l',ylim=c(-1000,500),xlab='ido', ylab='potenciál (uV)', main='Klaszterátlag')
#lines(c(1:length(data)/adatsec,al-mean(al),col='BLUE')
#lines(c(1:(felablak*2)),af-mean(af),col='RED')
}

if(db!=1 && db<100){lines(c(1:length(data)/adatsec),data-mean(data))
#lines(c(1:(felablak*2)),al-mean(al),col='BLUE')
#lines(c(1:(felablak*2)),af-mean(af),col='RED')
}


}



}#if abs...


}}
#rajzoljuk be pirossal az átlagot
#lines(c(1:length(data)/adatsec),(sum[ch,]-mean(sum[ch,]))/db ,col='RED')
dev.off()
