#SpikeSorting(fajlnev)

SpikeSorting<-function(fajlnev){
library("signal")
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
honnan.gamma<-30/NyqFreq #alső határ 
meddig.gamma<-100/NyqFreq #felső határ 

#MUA
honnan.mua<-300/NyqFreq
meddig.mua<-3000/NyqFreq

setwd(mentes)  ## set working directory
dirname<-paste(fajlnev,"_",start,'_','sec_est',sep='')  ## name a new directory
##dirname<-"Nov26c"
dir.create(dirname) #csinálunk egy mappát!!!  ## create the directory

#################SZŰRÉS############################
#szűrés paraméterei
setwd(parent)
setwd(dirname) 

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

#if(((x-1)dt+start)== ) ##ltp időszaka  


seek(fid,where=((q-1)*cs*mintf*dt+start*cs*mintf)*2,origin="start",rw="read",)

#a spike a teljes időszak közepén van 

adat<-readBin(fid, what='integer', size=2, n=cs*mintf*dt,endian="little",signed="TRUE")
adat<-matrix(adat,nrow=cs)
adat<-adat[csat.rend,]

#Butterworth filter


cat("defining filtering frequencies")
filter.gamma<-butter(1, c(honnan.gamma, meddig.gamma), type = "pass")
filter.mua<-butter(1, c(honnan.mua, meddig.mua), type = "pass")
#filter.gamma<-cheby1(5,3, W=c(honnan.gamma, meddig.gamma), type = "pass")
#filter.mua<-butter(5,3, W=c(honnan.mua, meddig.mua), type = "pass")
freqz(filter.gamma)
adat.filt.gamma<-t(apply(adat,1, function(x) filtfilt(filter.gamma,x) ))
adat.filt.mua<-t(apply(adat,1, function(x) filtfilt(filter.mua,x)))

matplot(t(adat.filt.gamma[1:50,1:9000]),t="l")

##### Spike detection
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

#   Gamma RMS 50 egysgnyi mozgóátlag
#melyik csatorna cuccait ábrázoljuk??
#csat<-8
updown<-adat[1:32,] #csak létrehozunk egy ekkora tömböt
updown.mua<-adat[1:32,]

rmsszamol<-function(adatsor){
  hossz<-50*adatsec #hány ms-ra simítunk
  rms<-sqrt( stats::filter(adatsor,rep(1/hossz,hossz), sides=2))
  return(rms)
}


adat.gamma.rms<-t(apply(adat.filt.gamma^2,1,rmsszamol))
adat.mua.rms<-t(apply(adat.filt.mua^2,1,rmsszamol))


#hist(adat.rms[1,],breaks=100)
#hist(adat.rms[2,],breaks=100)
#plot(adat.rms[1,],t='l')
#function for detecting up or down state
detekt<-function(x){
  #ave<-mean(x,na.rm=TRUE)
  ave<-1*mean(x,na.rm=TRUE)
  hataratlepes<-seq(along=x)[x >= ave]
  return(hataratlepes)
}


for(csat in 1:32){
cat(csat)

allapot<-c(rep(0,dt*mintf))
allapot.mua<-c(rep(0,dt*mintf))

hol<-detekt(adat.gamma.rms[csat,])
hol.mua<-detekt(adat.mua.rms[csat,])
allapot[hol]<-1
allapot.mua[hol.mua]<-1

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


#gördülős R ábra
legordul<-0
if(legordul==1){

a<-allapot
tt <- tktoplevel()
left <- tclVar(1)
oldleft <- tclVar(1)
right <- tclVar(2000)

f1 <- function(){
        lleft <- as.numeric(tclvalue(left))
        rright <- as.numeric(tclvalue(right))
        x <- seq(lleft,rright,by=1)
	par(mfrow=c(4,1), mar=c(1.5,1,0.5,0.5) ,oma=c(0.5,1,0.5,0.5) )
	#layout(matrix(c(1:4),4,1),widths=c(1,1,1,1), 		heights=c(3,2,2,1))
        plot(x/adatsec,y[x],ylim=range(y),t='l', xaxt="n")
	plot(x/adatsec,z[x],t='l', col='RED', xaxt="n")
	lines(x/adatsec,w[x], col='PURPLE')
	plot(x/adatsec,v[x],t='l', col='BLUE', xaxt="n")
	plot(x/adatsec,a[x],t='l', col='BLUE',xlab='ms')
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
} #legordul


abranev<-paste('UD_csat_',csat,'.png',sep='')
#abranev<-paste('/media/BA0ED4600ED416EB/agy/sCSD_git/leiras/pics/UD2_csat_',csat,'.png',sep='')

png(abranev, width=1600, height=800,pointsize = 30)
par(mfrow=c(4,1), mar=c(1.5,1,0.5,0.5) ,oma=c(0.5,1,2,0.5) )
	cim<-paste(csat,'. csatorna',sep='')
	
	layout(matrix(c(1:4),4,1),widths=c(1,1,1,1), 		heights=c(2,2,2,1))
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

} #up & down
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
return(paste("Spike Sorting ready","\n"))
}#SpikeSorting