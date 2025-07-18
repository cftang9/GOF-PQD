rm(list=ls(all=TRUE))
source("LW2014.R")

n = 100; 
B = 100000; 

LW_100 = array(,B); 
start.time = Sys.time()
for(b in 1:B){
  set.seed(0609250000+b)
  Data = array(runif(2*n),c(n,2))
  
  LW_PC = LW2(Data)
  
  LW_100[b] = LW_PC$Ad
  interval.time = (Sys.time()-start.time)/b; 
  if(b%%10==0){
    print(b); 
    print(Sys.time() + (B-b)*interval.time);   
  }
  
}

hist(LW_100)
#save(LW_100,file="LW_100.Rdata"); B=100k; 


quantile(LW_100,c(0.2,0.1,0.05,0.01,0.001))
# n=100 B=100k
#       20%       10%        5%        1%      0.1% 
# -2.143988 -2.502073 -2.842301 -3.608266 -4.925732



