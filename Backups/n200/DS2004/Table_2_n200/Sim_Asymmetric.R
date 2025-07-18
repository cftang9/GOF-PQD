rm(list = ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
library(Matrix)
source("DS2004.R")


n = 200; 
B = 1000; 

do_b = function(b){
  set.seed(081624000+b)
  Cv_inv = function(u,v,theta_1 = -0.12, theta_2 = 8.3){
    a = theta_1*(-2*v*sin(v*theta_2)); 
    b = theta_1*(1-v^2)*theta_2*cos(v*theta_2); 
    Cv_inv = (1+a+b - sqrt((1+a+b)^2 - 4*(a+b)*u))/(2*(a+b))
    return(Cv_inv)
  }
  Data = array(runif(2*n),c(n,2))
  Data = cbind(Cv_inv(Data[,1],Data[,2]),Data[,2])
  
  Start.Time = Sys.time()
  Start.Time.DS = Sys.time(); 
  DS = DS_C(Data)
  Duration.Time.DS = difftime(Sys.time(), Start.Time.DS, units='sec')
  write.table(data.frame(b=b, Start.Time = Start.Time, Current.Time=Sys.time(), 
                         Duration.Time.DS = Duration.Time.DS, DS.Rejection = as.numeric(DS$Reject), DS.TS = DS$TestStatistic, 
                         cores=detectCores()), file = "Sim_Asymmetric.csv", append = TRUE,
              row.names = FALSE, col.names = !file.exists("Sim_Asymmetric.csv"),
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
               library(matrixcalc)}
)
Test_b = foreach(b=1:B, 
                 .options.scheduling = "dynamic") %dopar% {do_b(b)}
stopCluster(cl)
