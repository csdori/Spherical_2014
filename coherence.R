#coherence calculation
#parent<-'/media/BA0ED4600ED416EB/agy/adat_acsadi/sil20_erdekes'
#fajlnev<-'p3d6.6whisker.dat'
#setwd(parent)
#csat.rend<-scan("chOrder32lin16lin16lin.txt")
#fid<-file(fajlnev,'rb')
#dt<-10
#seek(fid,where=((q-1)*cs*mintf*dt+start*cs*mintf)*2,origin="start",rw="read",)

#a spike a teljes időszak közepén van 

#adat<-readBin(fid, what='integer', size=2, n=cs*mintf*dt,endian="little",signed="TRUE")
#adat<-matrix(adat,nrow=cs)
#adat<-adat[csat.rend,]

#ered<-coherence(adat[3,],adat[5,],c(1,7),c(7,12))



coherence<-function(series1, series2, freq.low, freq.high){
  if (length(freq.low)!=length(freq.high)) stop("Length of vectors should really be the same")
  DataLength<-length(series1)/mintf
  series1<-ts(series1,frequency=mintf)
  series2<-ts(series2,frequency=mintf)
  spec.result<-spectrum(ts.union(series1, series2), fast=FALSE, taper=FALSE,
                spans = c(3,3),plot=FALSE)

  nlength<-length(freq.low)
  coh.matr<-array(0, c(nlength,4))
  colnames(coh.matr)<-c("FreqLow","FreqHigh","Coherence", "Phase")
  coh.matr[,1]<-freq.low
  coh.matr[,2]<-freq.high
  for(freq.band in 1:nlength){
    coh.matr[freq.band,3]<-mean(spec.result$coh[ceiling(freq.low[freq.band]*DataLength):ceiling(freq.high[freq.band]*DataLength)])
    coh.matr[freq.band,4]<-mean(spec.result$phase[ceiling(freq.low[freq.band]*DataLength):ceiling(freq.high[freq.band]*DataLength)])
  }
  return(coh.matr)
}
