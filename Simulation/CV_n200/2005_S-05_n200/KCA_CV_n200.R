rm(list=ls(all=TRUE))
source("KCA.r")
B = 10000; N = 200; 
KS = array(,B); CvM = KS; AD = KS; 
start_time = Sys.time()
set.seed(07022024)
for(b in 1:B){
  X = array(runif(2*N),c(N,2))
  temp_KCA = PQD.Dist.Joint(X)
  KS[b] = temp_KCA$TS$KS; 
  CvM[b] = temp_KCA$TS$CvM; 
  AD[b] = temp_KCA$TS$AD; 
  int_time = (Sys.time()-start_time)/b
  print(b)
  print(Sys.time()+int_time*(B-b))
}

quantile(KS,c(0.8, 0.9, 0.95, 0.99, 0.999))
#      80%       90%       95%       99%     99.9%
# 0.4949747 0.6010408 0.6717514 0.7778175 0.9192388
quantile(CvM,c(0.8, 0.9, 0.95, 0.99, 0.999))
#       80%        90%        95%        99%      99.9%
# 0.01882076 0.02918140 0.04130067 0.06918231 0.10747284 
quantile(AD,c(0.8, 0.9, 0.95, 0.99, 0.999))
#      80%       90%       95%       99%     99.9% 
# 0.6007341 0.8821933 1.1768929 1.8348176 2.7701549 
