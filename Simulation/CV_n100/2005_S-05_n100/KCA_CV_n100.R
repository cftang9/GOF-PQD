rm(list=ls(all=TRUE))
source("KCA.r")
B = 10000; N = 100; 
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
#   80%   90%   95%   99% 99.9%
# 0.500 0.600 0.675 0.800 0.925 
quantile(CvM,c(0.8, 0.9, 0.95, 0.99, 0.999))
#       80%        90%        95%        99%      99.9%
# 0.01757276 0.02770588 0.03876753 0.06646930 0.10687731 
quantile(AD,c(0.8, 0.9, 0.95, 0.99, 0.999))
#      80%       90%       95%       99%     99.9% 
# 0.5383999 0.7868719 1.0675148 1.7209787 2.7283629
