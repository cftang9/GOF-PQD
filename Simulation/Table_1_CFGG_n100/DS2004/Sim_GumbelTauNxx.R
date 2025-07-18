rm(list = ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
library(Matrix)
source("DS2004.R")

n = 100; 
B = 1000; 

do_b = function(b){
  set.seed(081624000+b)
  Tau = 0.0
  cop = gumbelCopula(iTau(gumbelCopula(),Tau))
  Data = rCopula(n,cop)
  Data = cbind(Data[,1],1-Data[,2])
  
  Start.Time = Sys.time()
  Start.Time.DS = Sys.time(); 
  DS = DS_C(Data)
  Duration.Time.DS = difftime(Sys.time(), Start.Time.DS, units='sec')
  write.table(data.frame(b=b, Start.Time = Start.Time, Current.Time=Sys.time(), 
                         Duration.Time.DS = Duration.Time.DS, DS.Rejection = as.numeric(DS$Reject), DS.TS = DS$TestStatistic, 
                         cores=detectCores()), file = "Sim_GumbelTauN00.csv", append = TRUE,
              row.names = FALSE, col.names = !file.exists("Sim_GumbelTauN00.csv"),
              sep = ",")
  return(NULL)
}

numCores <- detectCores() - 1 
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
