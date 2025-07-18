rm(list=ls(all=TRUE))

n = 10000; 
B = 10000; 
EL_Inf = array(,B); 
start.time = Sys.time()
for(b in 1:B){
  set.seed(0609250000+b)
  Nij = array(0,c(n+1,n+1)); 
  Nij[2:(n+1),2:(n+1)] = array(rnorm(n^2,0,sd=1/n),c(n,n))
  W = array(0,c(n+1,n+1)); 
  for(i in 2:(n+1)){
    for(j in 2:(n+1)){
      W[i,j] = W[i,j-1] + W[i-1,j] -W[i-1,j-1] + Nij[i,j]
    }
  }
  W0 = array(0,c(n+1,n+1)); 
  for(i in 2:n){
    for(j in 2:n){
      W0[i,j] = (max(0, n*(i-1)*W[n+1,j-1] + 
                        n*(j-1)*W[i-1,n+1] - 
                        n*n*W[i-1,j-1] - 
                       (i-1)*(j-1)*W[n+1,n+1]
      ))^2/((i-1)*(j-1)*(n-i+1)*(n-j+1))/n^2
    }
  }
  EL_Inf[b] = sum(W0)
  interval.time = (Sys.time()-start.time)/b; 
  if(b%%1==0){
    print(b); 
    print(Sys.time() + (B-b)*interval.time);   
  }
  
}

hist(EL_Inf)
#save(EL_Inf,file="EL_Inf.Rdata")


quantile(EL_Inf,c(0.8,0.9,0.95,0.975,0.99))
# n=2k B=2k
#      80%       90%       95%       99%     99.9% 
# 0.7433641 1.0732191 1.3811898 1.6839433 2.0999328 
# n=10k; B=10k; 
#      80%       90%       95%       99%     99.9% 
#0.7346073 1.0268457 1.3078578 2.0473108 2.9866454 
