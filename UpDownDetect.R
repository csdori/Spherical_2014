
#adat.gamma.rms.sum.s<-colMeans(adat.gamma.rms[1:16,])
#adat.gamma.rms.sum.v<-colMeans(adat.gamma.rms[17:32,])
#adat.mua.rms.sum.s<-colMeans(adat.mua.rms[1:16,])
#adat.mua.rms.sum.v<-colMeans(adat.mua.rms[17:32,])

UpDownDetect<-function(dataMua,dataGamma){
  UpStateMua<-detekt(dataMua)
  UpStateGamma<-detekt(dataGamma)
  allapotMua<-c(rep(0,length(dataMua)))
  allapotGamma<-c(rep(0,length(dataGamma)))
  allapotMua[UpStateMua]<-1
  allapotGamma[UpStateGamma]<-1
  felMua<-numeric()
  leMua<-numeric()
  for(i in 2:length(dataMua)){
    if(allapotMua[i-1]==0 && allapotMua[i]==1){
      felMua<-c(felMua,i+start*mintf)
    }
    if(allapotMua[i-1]==1 && allapotMua[i]==0){
      leMua<-c(leMua,i+start*mintf)
    }
  }
  felGamma<-numeric()
  leGamma<-numeric()
  
  for(i in 2:length(dataGamma)){
    if(allapotGamma[i-1]==0 && allapotGamma[i]==1){
      felGamma<-c(felGamma,i+start*mintf)
    }
    if(allapotGamma[i-1]==1 && allapotGamma[i]==0){
      leGamma<-c(leGamma,i+start*mintf)
    }
  }
  
  #throwing out the detected changes in up and down states, whoch are shorter, than 50 ms
  torol<-numeric()
  for(i in 1:length(felMua)){
    if(abs(leMua[i]-felMua[i])< (0.050*mintf))
      torol<-c(torol,i)
  }
  #felMua<-felMua[-torol]
  #leMua<-leMua[-torol]
  felMua.ido<-felMua/mintf
  leMua.ido<-leMua/mintf
  
  #throwing out the detected changes in up and down states, whoch are shorter, than 50 ms
  torol<-numeric()
  for(i in 1:length(felGamma)){
    if(abs(leGamma[i]-felGamma[i])< (0.050*mintf))
      torol<-c(torol,i)
  }
  #felGamma<-felGamma[-torol]
  #leGamma<-leGamma[-torol]
  felGamma.ido<-felGamma/mintf
  leGamma.ido<-leGamma/mintf
  
  return(list(felMua.ido,leMua.ido,felGamma.ido,leGamma.ido))
  
}




  