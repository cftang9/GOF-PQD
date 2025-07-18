rm(list=ls(all=TRUE))




PQD_GOF = function(Data,method="EL_A"){
  TS = 0; Time = 0; 
  if(method=="EL_A"){
    start.time = Sys.time()
    
    
    Time = difftime(Sys.time(), start.Time, units='sec')
  }
  return(TS=TS,Time=Time)
}


