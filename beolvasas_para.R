#beolvassi paraméterek


#
bm<-20 #betűméret az ábrákon

#melyik csatornákra vagyunk kiváncsiak?
#csatorna<-c(2:15,17:31,35:64)

##csatorna<-c(42:64)
##csatorna<-c(35:64)  ## thalamus channels:leave out last one to include dipoles in computation
##csatorna<-c(37:62)  ## thalamus channels:leave out last one to include dipoles in computation
csatorna<-c(37:62)  ## thalamus channels:leave out last one to include dipoles in computation
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




