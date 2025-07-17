source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF_Methods.R")

PQD_GOF = function(Data,method="ALL",Figure=T,log_Fig=T){
  n = length(Data[,1])
  if(Figure==T){
    par(mfrow=c(1,2))
    if(log_Fig==F){
        plot(Data[,1],Data[,2],xlab="X",ylab="Y")
      }
    if(log_Fig==T){
        plot(log(Data[,1]),log(Data[,2]),xlab="log X",ylab="log Y")
      }
    plot(rank(Data[,1])/(n+1),rank(Data[,2])/(n+1),
         xlim=c(0,1),ylim=c(0,1),
         main = "Scatterplot of pseudo-observations", 
         xlab="U", ylab="V")
    par(mfrow=c(1,1))
  }
  if(method=="EL_A" | method=="ALL"){
    start.time = Sys.time()
    temp = EL_Asy(Data)
    asy.EL = read.csv("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Methods/asy.EL.csv")
    asy.EL = asy.EL$x 
    EL_A = list(TS = temp$TS, 
                CV = as.numeric(quantile(asy.EL,0.95)), 
                pvalue = mean(asy.EL>=temp$TS), 
                Time = difftime(Sys.time(), start.time, units='sec'))
    print("EL_A")
    print(EL_A)
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
                CV = as.numeric(quantile(fin.EL.independent,0.95)), 
                pvalue = mean(fin.EL.independent>=temp$TS), 
                Time = difftime(Sys.time(), start.time, units='sec'))
    print("EL_F")
    print(EL_F)
  }
  if(method=="DS_04"| method=="ALL"){
    start.time = Sys.time()
    temp = DS_C(Data)
    
    DS_04 = list(TS = temp$TS, 
                 CV = temp$CV, 
                 pvalue = as.numeric(temp$pvalue), 
                 Time = difftime(Sys.time(), start.time, units='sec'))
    print("DS_04")
    print(DS_04)
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
                CV = as.numeric(quantile(fin.KS.independent,0.95)), 
                pvalue = mean(fin.KS.independent>=temp$TS), 
                Time = difftime(Sys.time(), start.time, units='sec'))
    print("S_05")
    print(S_05)
  }
  if(method=="GS_10" | method=="ALL"){
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
                 CV = as.numeric(quantile(fin.GS.independent,0.95)), 
                 pvalue = mean(fin.GS.independent>=temp$TS), 
                 Time = difftime(Sys.time(), start.time, units='sec'))
    print("GS_10")
    print(GS_10)
  }
  if(method=="GS_13"| method=="ALL"){
    start.time = Sys.time()
    temp = GS2013(Data)
    GS_13 = list(TS = temp$TS, 
                 CV = temp$CV, 
                 pvalue = temp$pvalue, 
                 Time = difftime(Sys.time(), start.time, units='sec'))
    print("GS_13")
    print(GS_13)
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
                 CV = as.numeric(quantile(fin.LW.independent,0.05)), 
                 pvalue = mean(fin.LW.independent<=temp$TS), 
                 Time = difftime(Sys.time(), start.time, units='sec'))
    print("LW_14")
    print(LW_14)
  }
  if(method=="LG_22"| method=="ALL"){
    start.time = Sys.time()
    temp = copula.test(Data,M=1000)
    LG_22 = list(TS = temp$TS, 
                 CV = temp$CV, 
                 pvalue = temp$pvalue, 
                 Time = difftime(Sys.time(), start.time, units='sec'))
    print("LG_22")
    print(LG_22)
  }
  
  if(method=="EL_A"){
    return(list(EL_A=EL_A))
  }
  if(method=="EL_F"){
    return(list(EL_F=EL_F))
  }
  if(method=="DS_04"){
    return(list(DS_04=DS_04))
  }
  if(method=="S_05"){
    return(list(S_05=S_05))
  }
  if(method=="GS_10"){
    return(list(GS_10=GS_10))
  }
  if(method=="GS_13"){
    return(list(GS_13=GS_13))
  }
  if(method=="LW_14"){
    return(list(LW_14=LW_14))
  }
  if(method=="LG_22"){
    return(list(LG_22=LG_22))
  }
  
  if(method=="ALL"){
    Table = array(,c(8,4))
    colnames(Table)<-c("Test statistic", "Critical value", "p-value", "Time")
    row.names(Table)<-c("EL_A", "EL_F", "DS_04", "S_05", "GS_10", "GS_13", "LW_14", "LG_22")
    Table[1,1] = EL_A$TS;   Table[1,2] = EL_A$CV;   Table[1,3] = EL_A$pvalue;   Table[1,4] = EL_A$Time; 
    Table[2,1] = EL_F$TS;   Table[2,2] = EL_F$CV;   Table[2,3] = EL_F$pvalue;   Table[2,4] = EL_F$Time; 
    Table[3,1] = DS_04$TS;  Table[3,2] = DS_04$CV;  Table[3,3] = DS_04$pvalue;  Table[3,4] = DS_04$Time; 
    Table[4,1] = S_05$TS;   Table[4,2] = S_05$CV;   Table[4,3] = S_05$pvalue;   Table[4,4] = S_05$Time; 
    Table[5,1] = GS_10$TS;  Table[5,2] = GS_10$CV;  Table[5,3] = GS_10$pvalue;  Table[5,4] = GS_10$Time; 
    Table[6,1] = GS_13$TS;  Table[6,2] = GS_13$CV;  Table[6,3] = GS_13$pvalue;  Table[6,4] = GS_13$Time; 
    Table[7,1] = LW_14$TS;  Table[7,2] = LW_14$CV;  Table[7,3] = LW_14$pvalue;  Table[7,4] = LW_14$Time; 
    Table[8,1] = LG_22$TS;  Table[8,2] = LG_22$CV;  Table[8,3] = LG_22$pvalue;  Table[8,4] = LG_22$Time; 
    print(Table)
    return(list(EL_A = EL_A, EL_F = EL_F, 
                DS_04 = DS_04, S_05 = S_05, 
                GS_10 = GS_10, GS_13 = GS_13, 
                LW_14 = LW_14, LG_22 = LG_22, Table=Table))
  }
}