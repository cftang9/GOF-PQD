rm(list = ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
source("GS2013.R")

Index = 0; 
n = 200; 
B = 1000; 

do_b = function(b){
  set.seed(081624000+b)
  Rho = -0.05*Index
  cop = normalCopula(Rho)
  Data = rCopula(n,cop)
  
  Start.Time = Sys.time(); 
  temp = GS2013(Data)
  
  return(list(b=b,TS=temp$TS, Rejection=temp$Rejection,
              Start.Time = Start.Time, Current.Time=Sys.time(), Diff.Time = difftime(Sys.time(), Start.Time, units='secs'),
              cores=detectCores()))
}

numCores <- detectCores()
cl <- makeCluster(numCores)
clusterExport(cl, varlist = c("do_b"))
registerDoParallel(cl)
clusterEvalQ(cl, 
             {
               library(parallel)
               library(foreach)
               library(doParallel)
               library(copula)}
)
Test_b = foreach(b=1:B, 
                 .options.scheduling = "dynamic") %dopar% {do_b(b)}
stopCluster(cl)

save(Test_b,file = paste0("Sim_NormalRhoN0",Index,".rdata"))