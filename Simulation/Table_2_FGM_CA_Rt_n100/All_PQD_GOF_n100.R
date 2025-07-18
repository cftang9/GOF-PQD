source("PQD_GOF_Methods_n100.r")

PQD_GOF_n100 = function(Data,method="EL_A"){
  if(method=="EL_A"){
    start.time = Sys.time()
    temp = EL_Asy(Data)
    Time = difftime(Sys.time(), start.time, units='sec')
  }
  if(method=="EL_F"){
    start.time = Sys.time()
    temp = EL_Fin_n100(Data)
    Time = difftime(Sys.time(), start.time, units='sec')
  }
  if(method=="DS-04"){
    start.time = Sys.time()
    temp = DS_C_n100(Data)
    Time = difftime(Sys.time(), start.time, units='sec')
  }
  if(method=="S-05"){
    start.time = Sys.time()
    temp = PQD.Dist.Joint.KS.n100(Data)
    Time = difftime(Sys.time(), start.time, units='sec')
  }
  if(method=="EAD"){
    start.time = Sys.time()
    temp = PQD.Dist.Joint.AD.n100(Data)
    Time = difftime(Sys.time(), start.time, units='sec')
  }
  if(method=="G-10"){
    start.time = Sys.time()
    temp = GS2010_LL_n100(Data)
    Time = difftime(Sys.time(), start.time, units='sec')
  }
  if(method=="LW-14"){
    start.time = Sys.time()
    temp = LW2_n100(Data)
    Time = difftime(Sys.time(), start.time, units='sec')
  }
  return(list(TS=temp$TS,Rejection=temp$Rejection,Time=Time))
}