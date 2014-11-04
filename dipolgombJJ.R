## Note: we are solving Poisson's eqn: laplacian(phi)=-I/sigma where I is the CSD.  This is in the same form as formula for the potential due to a solid distribution of charges: laplacian(phi)=-rho/epsilon. We use the integral solution and spherical harmonics.  In the code, epszilon corresponds to sigma.

#Ez a dipolos modell, melyet elvileg mindenféle sejtre alkalmazhatnánk..
#hátránya, hogy csak szimmetrikus számú csatonát képes látni... a dipol tagban...



#####################

#dirac delta
#belső tér
#mono
#fi=ro*R/epszilon
#dipol
#fi=ro*r*cos(theta)/(2*epszilon)

#külső
#mono
#fi=ro*R^2/(epszilon*r)
#dipol
#R^3*ro*cos(theta)/(2*epszilon*r^2)

###########
#gömbhéj
#belső
#mono
#fi=dR^2*ro/(2*epszilon)

#dipol
#fi=dR*r*0.5*ro*cos(theta)

#külső
#mono
#fi=dR^3/(3*epszilon)*ro/r

#dipol
#fi=dR^4/r^2*1/8*ro*cos(theta)

#############
#belső tér:
#I=4/3*pi*ro(R)/R
#fi(r)=ro(R)/(3*epszilon)*r*cos(theta)/R

#külső 
#Q=ro(R)*4/3*pi*R^2
#fi(r)=ro(R)(3*epszilon)*R^2/r^2*cos(theta)


#T.hej fura lesz ha a belső gömb térfogata nagyon nagy, elviszi a többit, de azért még mindig ez a jogosabb?

#interpolácioó

##NOTE: There is already a function with the same name in interpolacio.R: Is this for matrices while other for vectors? 
 interpol<-function(xbe,y,xki){
d1<-length(xki)
d2<-dim(y)[2]
out<-array(0,c(d1,d2))
for (i in 1: d2){
##inter<-splinefun(xbe,y[,i])
inter<-splinefun(xbe,y[,i],method="monoH.FC")
out[,i]<-inter(xki)
}
return(out)
}




legendre<-function(tavolsag,potencial){

tavol<-tavolsag #a gömb középpontja az elektródától d...

numshellsMax<-(b-a+1)/2
radiiONelectrodes <- 'YES'   ### choose 'YES' if radii of shells are on the electrodes
###radiiONelectrodes <- 'NO' ### choose 'NO' if radii are chosen so shells have equal widths

#U.extra<-DATLAG[(ch-dipolcsat+1):(ch+dipolcsat-1),]


ro<-1
epszilon<-1  # Note: epszilon is the permittivity constant sigma

#ch<-1


#agyi rétegek (sCSD rétegenként(mármint adottba tartozó sejtenként))  
#s1
s1<-c(1:16)
#v1 sejtek
v1<-c(17:32)

s2<-33 #üres
#thalamus sejtek
thal<-c(34:65)
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
#k

##ch2<-which(min(potencial)==potencial,arr.ind=TRUE)[1]## Added ch2 which has largest negative value of potential
minPot<-min(potencial[(a+1):(b-1),(ablak*mintf/2-20):(ablak*mintf/2+20)])
ch2<-which(minPot==potencial[(a+1):(b-1),(ablak*mintf/2-20):(ablak*mintf/2+20)],arr.ind=TRUE)[1]+c(a:b)[1] ## Added ch2 which has largest negative value of potential
dipolcsat<-min(numshellsMax,min(ch2-a+1,b-ch2+1))   ## Don't allow too many shells for middle channels


##dipolcsat<-min(ch2-a+1,b-ch2+1) #ahol van  #legalább egy belső héj van
#dipolcsat<-max(dipolcsat,1) #legalább egy belső héj van
##cat(paste('A ch2:',ch2))
##cat(paste('A ch:',ch))
##cat(paste('A dipolcsat:',dipolcsat))
##cat('\n')




theta<-vector(mode='numeric', length=2*dipolcsat-1)

#az elektródák pozíciója
el.poz<-vector(mode='numeric', length=2*dipolcsat-1)
el.poz<-c(1:(2*dipolcsat-1))*elektav-(dipolcsat)*elektav  ## centered at 0 with first entries negative
gomb.kp.poz<-0
## The angles in spherical coordinates theta measured from the positive z axis are between 0 and pi radians
#theta<-atan(tavol/abs(el.poz-gomb.kp.poz))
###theta<-atan2(tavol,(el.poz-gomb.kp.poz))
theta<-atan2(tavol,-(el.poz-gomb.kp.poz))  ### Note:  Neg. sign needed to ensure theta starts at zero upwards since el.poz are negative upwards

#áramok
#I.memb<-vector(mode='numeric', length=32)
#I.memb[16]<-1
#I.memb[17]<-1
#I.memb[1:15]<--c(1:15)/sum(c(1:15))
#I.memb[18:32]<--c(15:1)/sum(c(1:15))



T.dirac<-array(0,c(2*dipolcsat-1,2*dipolcsat-1))
T.hej<-array(0,c(2*dipolcsat-1,2*dipolcsat-1))

#mekkorák az elektródák által meghatározott sugarak?
##sugar.elfix<-vector(mode='numeric', length=2*dipolcsat-1)
##sugar.elfix<-sqrt(tavol^2+el.poz^2)
sugar.elfix<-vector(mode='numeric', length=dipolcsat)
sugar.elfix<-sqrt(tavol^2+el.poz[dipolcsat:(2*dipolcsat-1)]^2)  ## Radii of shells intersecting the electrodes in ascending order
#ezeket a sugarakat úra kell számolni
#hol metszik a gömbhélyak az elektródát? itt ez az el.poz


#állandó térfogatú eset
##sugar.V<-vector(mode='numeric', length=2*dipolcsat-1)
#sugar.V<-

#állandó vastagságú eset
#ez az állandó vastagság legyen mondjuk 50
##sugar.r<-vector(mode='numeric', length=2*dipolcsat-1)
sugar.r<-vector(mode='numeric', length=dipolcsat)
sugar.r<-tavol+(c(0:(dipolcsat-1))*50)  ## radius of each shell in ascending order: WHY exactly 50? 
elpoz.r<-vector(mode='numeric', length=2*dipolcsat-1)
elpoz.r[1:(dipolcsat-1)]<--sqrt(sugar.r*sugar.r-tavol*tavol)[(dipolcsat):2]
elpoz.r[-(1:(dipolcsat-1))]<-sqrt(sugar.r*sugar.r-tavol*tavol)[1:(dipolcsat)]
theta.r<-vector(mode='numeric', length=2*dipolcsat-1)
##theta.r<-atan(tavol/abs(elpoz.r-gomb.kp.poz))
###theta.r<-atan2(tavol,(elpoz.r-gomb.kp.poz))
theta.r<-atan2(tavol,-(elpoz.r-gomb.kp.poz))   ### Note:  Neg. sign needed to ensure theta starts at zero upwards since el.poz are negative upwards

#hol metszik a gömbhélyak az elektródát?


###############################################################
#idáig megvan... most még interpoláció kell és a transzfermátrixok felírása + ábrázolás....
#először legyen meg minden az elfixes esetre

#monopol
#j oszlopok, ja a többi gömbhéj
#i sorok, i ahol most vagyok

#a dipolcsat-ig kell mennie a dipólos indexnek
#a másiknak addig, amelyik oldalán hosszabb a rögzítés..áá, de inkább az us a dipólos indexig menjen, hülyén néz ki nem gömbszimmetrikus sejteknél

if (radiiONelectrodes=='YES'){
sugar<-sugar.elfix  ### Choose radii to match fixed electrodes
theta<-theta }       ### Choose radii to match fixed electrodes
else {
sugar<-sugar.r    ### Choose radii to match fixed shell widths
theta<-theta.r    ### Choose radii to match fixed shell widths
}

csdDirac.poz<-vector(mode='numeric', length=2*dipolcsat)
##csdDirac.poz.plot<-vector(mode='numeric', length=2*dipolcsat)
csdDirac.poz.plot<-vector(mode='numeric', length=2*dipolcsat-1)
csdHej.poz<-vector(mode='numeric', length=2*dipolcsat)
##csdHej.poz.plot<-vector(mode='numeric', length=2*dipolcsat)
csdHej.poz.plot<-vector(mode='numeric', length=2*dipolcsat-1)


csdDirac.poz[1:dipolcsat]<- -sugar[dipolcsat:1]
csdDirac.poz[(dipolcsat+1):(2*dipolcsat)]<- sugar[1:dipolcsat]
#Note: change the radius of the first sphere to the somawidth
##csdDirac.poz.plot<-csdDirac.poz
csdDirac.poz.plot[1:(dipolcsat-1)]<-csdDirac.poz[1:(dipolcsat-1)]
csdDirac.poz.plot[dipolcsat]<-0
csdDirac.poz.plot[(dipolcsat+1):(2*dipolcsat-1)]<-csdDirac.poz[(dipolcsat+2):(2*dipolcsat)]
##csdDirac.poz.plot[dipolcsat]<- -somawidth
##csdDirac.poz.plot[dipolcsat+1]<- somawidth


csdHej.poz[1:dipolcsat]<- -sugar[dipolcsat:1]
csdHej.poz[(dipolcsat+1):(2*dipolcsat)]<- sugar[1:dipolcsat]

##csdHej.poz.plot[dipolcsat]<- -somawidth
##csdHej.poz.plot[dipolcsat+1]<- somawidth
##csdHej.poz.plot[1:(dipolcsat-1)]<- -0.5*(sugar[(dipolcsat-1):1]+sugar[dipolcsat:2])
##csdHej.poz.plot[(dipolcsat+2):(2*dipolcsat)]<- 0.5*(sugar[1:(dipolcsat-1)]+sugar[2:dipolcsat])
csdHej.poz.plot[dipolcsat]<- 0
csdHej.poz.plot[1:(dipolcsat-1)]<- -0.5*(sugar[(dipolcsat-1):1]+sugar[dipolcsat:2])
csdHej.poz.plot[(dipolcsat+1):(2*dipolcsat-1)]<- 0.5*(sugar[1:(dipolcsat-1)]+sugar[2:dipolcsat])

for (i in 1:dipolcsat){
for(j in 1: dipolcsat){
if(j<=dipolcsat-i+1) {
T.dirac[i,j]<-ro/(2*sqrt(pi)*epszilon)*(sugar[j]^2)/sugar[dipolcsat-i+1] #external mono
if (j!=1) T.hej[i,j]<-ro/(6*sqrt(pi)*epszilon)*((sugar[j])^3-(sugar[j-1])^3)/sugar[dipolcsat-i+1]  #external mono
if(j==1) T.hej[i,j]<-ro/(6*sqrt(pi)*epszilon)*((sugar[j])^3)/sugar[dipolcsat-i+1]     #external mono
}

if(j>dipolcsat-i+1) {
T.dirac[i,j]<-ro/(2*sqrt(pi)*epszilon)*(sugar[j])
T.hej[i,j]<- ro/(4*sqrt(pi)*epszilon)*((sugar[j])^2-(sugar[j-1])^2)  #internal mono
}
}#j
}#i


#T alap (csak monopol rtagok)
T.alap<-array(0,c(dipolcsat,dipolcsat))
T.alap<-T.dirac[1:dipolcsat,1:dipolcsat]
## Make bottom rows symmetric for monopole
T.dirac[(2*dipolcsat-1):(dipolcsat+1),]<-T.dirac[1:(dipolcsat-1),]
T.hej[(2*dipolcsat-1):(dipolcsat+1),]<-T.hej[1:(dipolcsat-1),]


#  dipol (itt ki kell hagyni a középső csatornát...)
for (i in 1:(dipolcsat)){
for(j in 1:(dipolcsat-1) ){
if(j<=dipolcsat-i){      #external dipol
T.dirac[i,j+dipolcsat]<-ro/(2*sqrt(3*pi)*epszilon)*((sugar[j+1])^3)/((sugar[dipolcsat-i+1])^2)*cos(theta[i])
T.hej[i,j+dipolcsat]<-ro/(8*sqrt(3*pi)*epszilon)*((sugar[j+1])^4-(sugar[j])^4)/((sugar[dipolcsat-i+1])^2)*cos(theta[i]) #external dipol
}

if(j>dipolcsat-i){     #internal dipol
T.dirac[i,j+dipolcsat]<-ro/(2*sqrt(3*pi)*epszilon)*sugar[dipolcsat-i+1]*cos(theta[i]) 
T.hej[i,j+dipolcsat]<-ro/(2*sqrt(3*pi)*epszilon)*(sugar[j+1]-sugar[j])*sugar[dipolcsat-i+1]*cos(theta[i]) #internal dipol
}
}#j
}#i


## Make the bottom right entries symmetric (with a negative sign) to those in the top right.  The negative sign comes from the cosine term since the angles are greater than pi/2
T.dirac[(2*dipolcsat-1):(dipolcsat+1),(dipolcsat+1):(2*dipolcsat-1)]<- -T.dirac[1:(dipolcsat-1),(dipolcsat+1):(2*dipolcsat-1)]
T.hej[(2*dipolcsat-1):(dipolcsat+1),(dipolcsat+1):(2*dipolcsat-1)]<- -T.hej[1:(dipolcsat-1),(dipolcsat+1):(2*dipolcsat-1)]
#image(T.dirac)



I.forras<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.forras.soma<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
##I.mono<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.mono<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)  ### Note: changed locations of CS
I.mono.soma<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)  ### Note: changed locations of CS
##I.dipol<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.dipol<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)  ### Note: changed locations of CS
I.dipol.soma<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)  ### Note: changed locations of CS
##I.egyutt<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1) 
I.egyutt<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1) ### Note: changed locations of CSD
I.egyutt.soma<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1) ### Note: changed locations of CSD
I.egyutt.soma.plot<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1) 

I.forras.hej<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.forras.hej.soma<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
##I.mono.hej<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.mono.hej<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.mono.hej.soma<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
##I.dipol.hej<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.dipol.hej<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.dipol.hej.soma<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1) 
##I.egyutt.hej<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.egyutt.hej<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.egyutt.hej.soma<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.egyutt.hej.plot<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)
I.egyutt.hej.soma.plot<-matrix(rep(0,(2*dipolcsat-1)*ablak*mintf),nrow=2*dipolcsat-1)

U.extra<-potencial[(ch2-dipolcsat+1):(ch2+dipolcsat-1),]  ## Note ch replaced with ch2 so that middle electrode has the largest negative potential
##U.extra<-potencial[(ch-dipolcsat+1):(ch+dipolcsat-1),]
#U.extra<-DATLAG[(ch-dipolcsat+1):(ch+dipolcsat),]


if (radiiONelectrodes=='YES'){
I.forras<-solve(T.dirac)%*%U.extra    ## Choose for radii matching fixed electrodes 
I.forras.hej<-solve(T.hej)%*%U.extra}  ## Choose for radii matching fixed electrodes
else {
U.extra.r<-interpol(el.poz,U.extra,elpoz.r) ## Choose for fixed shell width
I.forras<-solve(T.dirac)%*%U.extra.r    ## Choose for fixed shell width
I.forras.hej<-solve(T.hej)%*%U.extra.r  ## Choose for fixed shell width
}
#visszaszámoljuk 
#a monopól része

I.mono[1:dipolcsat,]<- 1/sqrt(4*pi)*I.forras[dipolcsat:1,]
I.mono[(2*dipolcsat-1):(dipolcsat),]<- 1/sqrt(4*pi)*I.forras[dipolcsat:1,]
##I.mono[(2*dipolcsat):(dipolcsat+1),]<- 1/sqrt(4*pi)*I.forras[dipolcsat:1,] ### Note: changed locations of CSD
 
I.mono.hej[1:dipolcsat,]<- 1/sqrt(4*pi)*I.forras.hej[dipolcsat:1,]
##I.mono.hej[(2*dipolcsat):(dipolcsat+1),]<- 1/sqrt(4*pi)*I.forras.hej[dipolcsat:1,]
I.mono.hej[(2*dipolcsat-1):(dipolcsat),]<- 1/sqrt(4*pi)*I.forras.hej[dipolcsat:1,]

I.dipol[1:(dipolcsat-1),]<- sqrt(3/(4*pi))*I.forras[(2*dipolcsat-1):(dipolcsat+1),]*cos(theta[1:(dipolcsat-1)])
##I.dipol[(2*dipolcsat-1):(dipolcsat+1),]<- -sqrt(3/(4*pi))*I.forras[(2*dipolcsat-1):(dipolcsat+1),] # minus sign accounts for cos(pi) term in formula for the CSD
I.dipol[(2*dipolcsat-1):(dipolcsat+1),]<- sqrt(3/(4*pi))*I.forras[(2*dipolcsat-1):(dipolcsat+1),]*cos(theta[(2*dipolcsat-1):(dipolcsat+1)]) # minus sign accounts for cos(pi) term in formula for the CSD ### Note: changed locations of CSD
I.dipol.hej[1:(dipolcsat-1),]<- sqrt(3/(4*pi))*I.forras.hej[(2*dipolcsat-1):(dipolcsat+1),]*cos(theta[1:(dipolcsat-1)])
I.dipol.hej[(2*dipolcsat-1):(dipolcsat+1),]<- sqrt(3/(4*pi))*I.forras.hej[(2*dipolcsat-1):(dipolcsat+1),]*cos(theta[(2*dipolcsat-1):(dipolcsat+1)]) # minus sign accounts for cos(pi) term in formula for the CSD


I.egyutt<-I.mono+I.dipol  # This is the CSD
I.egyutt.plot<- I.egyutt
### change the diameter of the first sphere to approximate the soma width
I.egyutt.plot[dipolcsat:(dipolcsat+1),]<-(I.egyutt.plot[dipolcsat:(dipolcsat+1),])*(sugar[1]/somawidth)^2 ##CSD for plotting

I.egyutt.hej<-I.mono.hej+I.dipol.hej
I.egyutt.hej.plot<- I.egyutt.hej
### change the diameter of the first sphere to approximate the soma width
I.egyutt.hej.plot[dipolcsat:(dipolcsat+1),]<-(I.egyutt.hej.plot[dipolcsat:(dipolcsat+1),])*(sugar[1]/somawidth)^3 ##CSD for plotting
#image(I.egyutt)

#hejvastagsag az abrazolásnál
#10 umerenként kéne mindkét irányba

##hatar.also<-floor(min(elpoz.r)/10)*10
##hatar.felso<-ceiling(max(elpoz.r)/10)*10
##felbontas<-seq(hatar.also,hatar.felso, by=10)

#I.dirac.ip<-array(0,c(50,dim(I.egyutt)[2]))
#I.dirac.ip.hej<-array(0,c(50,dim(I.egyutt)[2]))

##I.dirac.ip<-interpol(elpoz.r,I.egyutt,felbontas)
##I.hej.ip<-interpol(elpoz.r,I.egyutt.hej,felbontas)


###if (radiiONelectrodes=='YES'){
##hatar.also<-floor(min(el.poz)/10)*10
hatar.felso<-ceiling(max(el.poz)/10)*10
##hatar.felso<-ceiling(max(sugar)/10)*10
felbontas<-seq(-hatar.felso,hatar.felso, by=10)
I.dirac.ip<-interpol(el.poz,I.egyutt,felbontas)
##I.dirac.ip<-interpol(csdDirac.poz.plot,I.egyutt.plot,felbontas)
I.hej.ip<-interpol(el.poz,I.egyutt.hej,felbontas)
##I.hej.ip<-interpol(csdHej.poz.plot,I.egyutt.hej.plot,felbontas)
###}
###else {
###hatar.also<-floor(min(elpoz.r)/10)*10
###hatar.felso<-ceiling(max(elpoz.r)/10)*10
###felbontas<-seq(hatar.also,hatar.felso, by=10)
###I.dirac.ip<-interpol(elpoz.r,I.egyutt,felbontas)
###I.hej.ip<-interpol(elpoz.r,I.egyutt.hej,felbontas)}  

kimenet<-new.env()
kimenet$I.dirac<-I.egyutt
kimenet$I.dirac.plot<-I.egyutt.plot
kimenet$I.hej<-I.egyutt.hej
kimenet$I.hej.plot<-I.egyutt.hej.plot
kimenet$I.dirac.ip<-I.dirac.ip
kimenet$I.hej.ip<-I.hej.ip
kimenet$hejvas<-sugar
kimenet$dipolcsat<-dipolcsat
kimenet$elektav<-elektav
kimenet$felbontas<-felbontas
kimenet$Tdirac<-T.dirac
kimenet$Thej<-T.hej
kimenet$A0A1dirac<-I.forras
kimenet$A0A1hej<-I.forras.hej
kimenet$ch2 <- ch2
kimenet$minPot<-minPot
kimenet$el.poz<-el.poz
return(kimenet)
}
