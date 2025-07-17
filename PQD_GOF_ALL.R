source("PQD_GOF_Methods.r")

PQD_GOF_ALL = function(Data,method="ALL",Figure=F){
  n = length(Data[,1])
  if(Figure==T){
    par(mfrow=c(1,2))
    plot(Data[,1],Data[,2],xlab="X",ylab="Y")
    
    plot(rank(Data[,1])/(n+1),rank(Data[,2])/(n+1),
         xlim=c(0,1),ylim=c(0,1),
         xlab="U", ylab="V")
    par(mfrow=c(1,1))
  }
  if(method=="EL_A" | method=="ALL"){
    start.time = Sys.time()
    temp = EL_Asy(Data)
    load("asy.EL.rdata")
    EL_A = list(TS = temp$TS, 
                pvalue = mean(asy.EL>temp$TS), 
                Time = difftime(Sys.time(), start.time, units='sec'))
  }
  if(method=="EL_F" | method=="ALL"){
    start.time = Sys.time()
    temp = EL_Fin(Data)
    
    B0 = 10000 
    fin.EL.independent = array(,B0); 
    for(b0 in 1:B0){
      set.seed(070125000+b0)
      Data0 = array(runif(2*n),c(n,2)); 
      fin.EL.independent[b0] = EL_Fin(Data0)$TS
    }
    EL_F = list(TS = temp$TS, 
                pvalue = mean(fin.EL.independent>temp$TS), 
                Time = difftime(Sys.time(), start.time, units='sec'))
  }
  if(method=="DS_04"| method=="ALL"){
    start.time = Sys.time()
    temp = DS_C(Data)
    
    DS_04 = list(TS = temp$TS, 
                 pvalue = temp$pvalue, 
                 Time = difftime(Sys.time(), start.time, units='sec'))
  }
  if(method=="S_05" | method=="ALL"){
    start.time = Sys.time()
    temp = PQD.Dist.Joint.KS(Data)
    
    B0 = 10000; 
    fin.KS.independent = array(,B0); 
    for(b0 in 1:B0){
      set.seed(070125000+b0)
      Data0 = array(runif(2*n),c(n,2)); 
      fin.KS.independent[b0] = PQD.Dist.Joint.KS(Data0)$TS
    }
    
    S_05 = list(TS = temp$TS, 
                pvalue = mean(fin.KS.independent>temp$TS), 
                Time = difftime(Sys.time(), start.time, units='sec'))
  }
  if(method=="G_10" | method=="ALL"){
    start.time = Sys.time()
    temp = GS2010_LL(Data)
    
    B0 = 10000 
    fin.GS.independent = array(,B0); 
    for(b0 in 1:B0){
      set.seed(070125000+b0)
      Data0 = array(runif(2*n),c(n,2)); 
      fin.GS.independent[b0] = GS2010_LL(Data0)$TS
    }
    
    GS_10 = list(TS = temp$TS, 
                 pvalue = mean(fin.GS.independent>temp$TS), 
                 Time = difftime(Sys.time(), start.time, units='sec'))
  }
  if(method=="GS_13"| method=="ALL"){
    start.time = Sys.time()
    temp = GS2013(Data)
    GS_13 = list(TS = temp$AD, 
                 pvalue = temp$pvalue, 
                 Time = difftime(Sys.time(), start.time, units='sec'))
  }
  if(method=="LW_14"| method=="ALL"){
    start.time = Sys.time()
    temp = LW2(Data)
    
    B0 = 100000 
    fin.LW.independent = array(,B0); 
    for(b0 in 1:B0){
      set.seed(070125000+b0)
      Data0 = array(runif(2*n),c(n,2)); 
      fin.LW.independent[b0] = LW2(Data0)$TS
    }
    
    LW_14 = list(TS = temp$TS, 
                 pvalue = mean(fin.LW.independent<temp$TS), 
                 Time = difftime(Sys.time(), start.time, units='sec'))
    
  }
  if(method=="LG_22"| method=="ALL"){
    start.time = Sys.time()
    temp = copula.test(Data,M=1000)
    LG_22 = list(TS = temp$ad.test, 
                 pvalue = temp$pvalue, 
                 Time = difftime(Sys.time(), start.time, units='sec'))
  }
  return(list(EL_A = EL_A, EL_F = EL_F, DS_04 = DS_04, S_05 = S_05, 
              GS_10 = GS_10, GS_13 = GS_13, LW_14 = LW_14, LG_22 = LG_22))
}

# n = 50
# W = PQD_GOF_ALL(array(runif(2*n),c(n,2)))
# W
