#beolvassi paraméterek

#Hol van az adat?
fajlnev<-'p3d6.6.dat'
#fajlnev<-'p3d6.6.dat'
#fajlnev2<-'p3d6.6whisker'  ##used when using Acsadi's own .clu and .res files

#whics machine do I use:
#otthon
WhereamI<-'kfki' #'otthon' #kfki


# Up_down stat, first spike
FirstSpikeOnly<-"no" #"yes" #no
state.wanted<-1 #"Up"=1, "Down"=0, "Together"=3

#Do you want to calculate the coherence
CalcCoh<-"No" # "yes" #"No"
#Do you want to run the clustering on a specific data?
DoClustering<-"No" #Yes"#'Yes' #No
# Uo and down state seperation based on the sensorymothor -4 or visual cortex - 5
whichUpDown<-5


#Are the distances known: 0 no, 1 -yes
later<-0
#Name of Simulation
NameofSimulation<-paste(fajlnev,"_ver1",sep="")


if (WhereamI=='otthon'){
  parent<-'/media/BA0ED4600ED416EB/agy/adat_acsadi/sil20_erdekes'
  forras1<-'/media/BA0ED4600ED416EB/agy/Spherical_sCSD_2015/'
  mentes<-paste("/media/BA0ED4600ED416EB/agy/Spherical_sCSD_2015/",NameofSimulation,sep="")
  setwd('/media/BA0ED4600ED416EB/agy/Spherical_sCSD_2015/')
  
} 
if (WhereamI=='kfki'){
  parent<-'/home/csdori/sil20_erdekes'
  mentes<-paste("/home/csdori/Spherical_sCSD_2014/Spherical_2014/",NameofSimulation,sep="")
  forras1<-"/home/csdori/Spherical_sCSD_2014/Spherical_2014/"
  
}


if (WhereamI=='Jozsikfki'){
  parent<-"/home/jalics/sil20_erdekes"
  forras1<-"/home/jalics/sCSD_git" 
  mentes<-paste("/home/jalics/ACSADI_elem_09",NameofSimulation,sep="")
  setwd("/home/jalics/sCSD_git")  ## on tauri: Working directory for loading R files
} 

if(file.exists(mentes)==FALSE) dir.create(mentes)


################################################
#####################################x
##############################x

#
bm<-20 #betűméret az ábrákon

#melyik csatornákra vagyunk kiváncsiak?
csatorna<-c(2:15,18:31,35:64)

##csatorna<-c(42:64)
##csatorna<-c(35:64)  ## thalamus channels:leave out last one to include dipoles in computation
##csatorna<-c(37:62)  ## thalamus channels:leave out last one to include dipoles in computation
#csatorna<-c(37:62)  ## thalamus channels:leave out last one to include dipoles in computation
#csatorna<-c(2:31)
csathossz<-length(csatorna)

eltav<-100 #elektródák közti távolság
#rajzoljon-e sok cuccot?
rajzol='nem'
#cat('honnan olvastam be?')


cs<-65  #csatornák száma 

#mekkora időablakkal nézzük (sec)? 
ABLAK<-c(0.02,0.004)  #a klaszteratlagnal stb abrazolas idobeli hossza  ##also in tuskerajz.R
ablak<-0.02 #ide a nagyobb időablak kell, utána majd a plotolásnál átíródik kisebbre
mintf<-20000 #mintavételezési frekvencia 
adatsec<-20

#a becsült távolság gömbszimmetrikus sejtekre
tavg<-20


#utan<-20
utan<-13
#elott<-9
elott<-6
hossz<-utan+1+elott

#honnan olvassuk be
#cat('honnan olvastam be?
start<-1 #(sec-ben,10-snként változtatható,5-re végződő...) #

#agyi rétegek (sCSD rétegenként(mármint adottba tartozó sejtenként))  
#s1
s1<-c(1:16)  ## create vector for s1
#v1 sejtek
v1<-c(17:32) ## create vector for v1 

s2<-33 #üres
#thalamus sejtek
thal<-c(34:65)  ## create vector for thalamus

setwd(parent)
#csatornasorrend
csat.rend<-scan("chOrder32lin16lin16lin.txt")

setwd(forras1)
source("sCSD_dipolJJ.R")
