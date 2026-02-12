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
  return(list(TS=temp$TS,Rejection=temp$Rejection,Time=Time))
}