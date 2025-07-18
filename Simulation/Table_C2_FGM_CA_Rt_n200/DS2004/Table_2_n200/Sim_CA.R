rm(list = ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
library(Matrix)
library(Distributacalcul)
source("DS2004.R")

Index = 0; 
n = 200; 
B = 1000; 

do_b = function(b){
  Theta = 0.2*Index
  Data = crBivariateCA(n, seed = 081624000+b, dependencyParameter=Theta)
  Data[,1] = 1-Data[,1]; 
  
  Start.Time = Sys.time()
  Start.Time.DS = Sys.time(); 
  DS = DS_C(Data)
  Duration.Time.DS = difftime(Sys.time(), Start.Time.DS, units='sec')
  write.table(data.frame(b=b, Start.Time = Start.Time, Current.Time=Sys.time(), 
                         Duration.Time.DS = Duration.Time.DS, DS.Rejection = as.numeric(DS$Reject), DS.TS = DS$TestStatistic, 
                         cores=detectCores()), file = "Sim_CA_N00.csv", append = TRUE,
              row.names = FALSE, col.names = !file.exists("Sim_CA_N00.csv"),
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
               library(copula)
               library(matrixcalc)
               library(Distributacalcul)
               }
)
Test_b = foreach(b=1:B, 
                 .options.scheduling = "dynamic") %dopar% {do_b(b)}
stopCluster(cl)
