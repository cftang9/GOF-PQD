rm(list = ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
library(Matrix)
library(Distributacalcul)
source("DS2004.R")

Index = 3; 
n = 200; 
B = 1000; 

do_b = function(b){
  set.seed(081624000+b)
  Theta = 0.2*Index
  rCA_coupla = function(n,theta){
    u = runif(n); v = runif(n); 
    if(theta == 0){
      rCA_coupla = cbind(u,v)
    }
    if(theta>0 & theta<1){
      I1 = c(v>0 & v< u^(1-theta)*(1-theta)); 
      I2 = c(v>=u^(1-theta)*(1-theta) & v<u^(1-theta)); 
      I3 = c(v>=u^(1-theta) & v<1); 
      Inv = (  I1 * u^(theta)*v/(1-theta)
               + I2 * pmax(u+v-1,0)
               + I3 * v^(1/(1-theta))
      )
      rCA_coupla = cbind(u*(1-I2)+pmax(u+v-1,0)*I2, Inv)
    }
    if(theta == 1){
      w = pmax(u+v-1,0)
      rCA_coupla = cbind(w,w)
    }
    return(rCA_coupla)
  }
  Data = rCA_coupla(n,theta=Theta)
  Data[,1] = 1-Data[,1]; 
  
  Start.Time = Sys.time()
  Start.Time.DS = Sys.time(); 
  DS = DS_C(Data)
  Duration.Time.DS = difftime(Sys.time(), Start.Time.DS, units='sec')
  write.table(data.frame(b=b, Start.Time = Start.Time, Current.Time=Sys.time(), 
                         Duration.Time.DS = Duration.Time.DS, DS.Rejection = as.numeric(DS$Reject), DS.TS = DS$TestStatistic, 
                         cores=detectCores()), file = "Sim_CA_N06.csv", append = TRUE,
              row.names = FALSE, col.names = !file.exists("Sim_CA_N06.csv"),
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
