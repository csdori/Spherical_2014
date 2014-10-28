#isi


#isikiírás
isiname<-paste("isi_cs",ch,"_k_", k,sep = "")
write.table(isi,isiname)
if(length(isi) <3) break
  isi2<-numeric()
  ms2<-0
  ms10<-0
  for(w in 1:length(isi)){
    if(isi[w]<20) isi2<-c(isi2,isi[w]) 
    if(isi[w]<2) ms2<-ms2+1
    if(isi[w]<10) ms10<-ms10+1
  }
  
  
  #isirajzolás
  #if(length(isi2)>2){
  isiname<-paste("isi_csat_",ch,"_k_", k,".png", sep = "")
  png(pointsize=bm,filename = isiname)
  par(mfrow=c(1,2))
  cim<-paste('A ',ch,'. csatorna ',k,' . klaszterének ISI hisztogrammja', sep = "")
  #hist(isi2,breaks=c(seq(0,max(isi2),0.5),max(isi2)),main=cim,xlab='idő (ms)', ylab=('tüskék száma'),freq=TRUE,col='gray')
  hist(isi,breaks=c(seq(0,max(isi),5),max(isi)),col='gray',xlim=c(0,50),xlab='isi (ms)')
 
  #összes tüske db:
  ossz.tuske<-paste("összes tüske száma: ",length(isi)+1," db",sep=""  )
  
  #2ms-on belüli tüskék száma
  ms2.tuske<-paste("2 ms-on belüli tüskék száma: ",ms2," db",sep=""  )
  
  #10ms-on belüli tüskék száma
  ms10.tuske<-paste("10 ms-on belüli tüskék száma: ",ms10," db",sep=""  )
  
  #2ms/10ms
  arany.tuske<-paste("2 és 10 ms-on belüli tüskék aránya: ",round(ms2/ms10,digits=3),sep="")
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  legend("topright",c(ossz.tuske,ms2.tuske,ms10.tuske,arany.tuske),lty=0,cex=0.6)

  dev.off()

