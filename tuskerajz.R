

#cat("Only first spike of bursts taken into account")


ClusterAverage<-function(FirstSpikeOnly="no",state.wanted=3){
  
  if(FirstSpikeOnly=="yes") cat("Only first spike of bursts taken into account")
  
  
  MiMiName<-paste(mentes,"/MIMI_ch",ch,sep='')
  MiMi<-read.table(MiMiName)
  start<-1
  #ABLAK<-c(0.02,0.004)
  ##idoskal<-2 #itt alapból for ciklus van.. de hát...
  idoskal<-2 #itt alapból for ciklus van.. de hát...
  ablak<-ABLAK[idoskal]   #az ábrák, számolások időablakának hossza
  felablak<-ablak/2*mintf #adatpontokban az időablak hosszának fele
  
  #creating variables
  
  A<-1   
  B<-length(wow)
  
  sum<-matrix(c(rep(0,2*felablak*cs)),nrow=cs)
  negyzet<-matrix(c(rep(0,2*felablak*cs)),nrow=cs)
  szorasnegyzet<-matrix(c(rep(0,2*felablak*cs)),nrow=cs)
  szoras<-matrix(c(rep(0,2*felablak*cs)),nrow=cs)
  t<-matrix(c(rep(0,2*felablak*cs)),nrow=cs)
  db<-0   #tüskék száma az adott klaszterben
  atlag<-matrix(c(rep(0,2*felablak*cs)),nrow=cs) #csak 16 csatorna esetén adja ki az összeset!!! 
  isi<-numeric()
  idopontok<-numeric()
  elozo<-0
  
  
  
  
  
  
  
  
  
  
  setwd(parent)
  
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
    
    if (ch<33) state.l<-MiMi[l,5]
    if(ch<33 && state.l!=state.wanted && state.wanted!=3) next
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
            data<-adat[ch,]     #ez mér kell?
            
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
          data<-adat[ch,]     #ez mér kell?
          
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
      
      
    }
    
    
  }
  #rajzoljuk be pirossal az átlagot
  #lines(c(1:length(data)/adatsec),(sum[ch,]-mean(sum[ch,]))/db ,col='RED')
  dev.off()
  
  if(db!=0 && db!=1){
    ATLAG<-sum/db
    
    DATLAG<-data.matrix(ATLAG)
    DATLAG<-DATLAG-rowMeans(DATLAG[,-((ablak/4*mintf):(ablak*3/4*mintf))])
  } else {ATLAG<-NA
          DATLAG<-NA
  
  }
  
  
  ########calculating acf, isi etc.....
  
  cat('idopontok elmentese\n')
  #idopontok elmentese
  idopon.nev<-paste('ido_',ch,'_k_',k,sep='')
  write.table(idopontok,idopon.nev)
  #ide kéne valami acf
  #acf(idopontok, type="correlation") ####
  #legyen a fel ms-os binelésű
  
  ido<-numeric()
  ido<-idopontok
  if(length(ido)==0) break
  rm(idopontok)
  
  bhossz<-0.5 #ms
  
  #ahhoz, hogy tudjunk kerekíteni egész számra szorozzunk meg mindent kettővel
  #és az ido-t szorozzuk meg ezerrel, hogy ms-ben legyen...
  
  ido.max<-max(ido)
  idopontok<-seq(from=0, to=2*ido.max*1000, by=2*bhossz)
  ido2<-2*1000*ido
  
  ido.ts<-rep(0,length(idopontok ))
  for(i in 1:length(ido)){
    ido.ts[ceiling(ido2[i])]<-ido.ts[floor(ido2[i])]+1
  }
  
  idosor<-matrix(c(idopontok,ido.ts),ncol=2)
  lagmeret<-40
  
  acfeleje<-acf(idosor[,2],demean=FALSE,ylim=c(0,0.2),lag.max=lagmeret)
  acfvege<-acf(rev(idosor[,2]),demean=FALSE,ylim=c(0,0.1),lag.max=lagmeret)
  
  acfmain<-paste("acf fél ms-os bineléssel (",ch,". csatorna", k,". klaszter)" , sep = "")
  acfname<-paste("acf_csat_",ch,"_k_", k,".png", sep = "")
  #png(pointsize=bm,filename = acfname, width = 700, height = 700)
  #barplot(c(rev(acfvege$acf),acfeleje$acf)*sum(idosor[,2]),xlim=c(0,2*lagmeret), main='acf fél ms-os bineléssel')
  #dev.off()
  return(list(ATLAG=ATLAG,DATLAG=DATLAG,db=db,isi=isi))
}

