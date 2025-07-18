rm(list=ls(all=TRUE))
source("LW2014.R")

n = 200; 
B = 100000; 

LW_200 = array(,B); 
start.time = Sys.time()
for(b in 1:B){
  set.seed(0609250000+b)
  Data = array(runif(2*n),c(n,2))
  
  LW_PC = LW2(Data)
  
  LW_200[b] = LW_PC$Ad
  interval.time = (Sys.time()-start.time)/b; 
  if(b%%10==0){
    print(b); 
    print(Sys.time() + (B-b)*interval.time);   
  }
  
}

hist(LW_200)
#save(LW_200,file="LW_200.Rdata"); B=100k; 


quantile(LW_200,c(0.2,0.1,0.05,0.01,0.001))
# n=200 B=100k
#       20%       10%        5%        1%      0.1% 
# -2.447330 -2.805699 -3.137567 -3.899454 -5.612593 



