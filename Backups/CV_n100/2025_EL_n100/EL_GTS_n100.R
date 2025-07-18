rm(list=ls(all=TRUE))

n = 100; 
B = 10000; 

EL_100 = array(,B); 
start.time = Sys.time()
for(b in 1:B){
  set.seed(0609250000+b)
  Data = array(runif(2*n),c(n,2))
  X = Data[,1]; Y = Data[,2]; 
  n = length(X); 
  Hn = array(,n); 
  An11 = Hn; An12 = Hn; An21 = Hn; An22 = Hn;
  A011 = Hn; A012 = Hn; A021 = Hn; A022 = Hn;
  An011 = Hn; An012 = Hn; An021 = Hn; An022 = Hn;
  LLn1 = array(0,n); 
  for(i in 1:n){
    IFn = c(X<=X[i]); IGn = c(Y<=Y[i]); 
    Hn[i] = mean(IFn*IGn); 
    An11[i] = Hn[i]; 
    An12[i] = mean(IFn) - Hn[i]; 
    An21[i] = mean(IGn) - Hn[i]; 
    An22[i] = 1 - An11[i] - An12[i] - An21[i]; 
    A011[i] = (An11[i]+An12[i])*(An11[i]+An21[i]); 
    A012[i] = (An11[i]+An12[i])*(1-(An11[i]+An21[i])); 
    A021[i] = (1-(An11[i]+An12[i]))*(An11[i]+An21[i]); 
    A022[i] = (1-(An11[i]+An12[i]))*(1-(An11[i]+An21[i])); 
    An011[i] = max(A011[i],An11[i]);
    An012[i] = min(A012[i],An12[i]);
    An021[i] = min(A021[i],An21[i]);
    An022[i] = max(A022[i],An22[i]);
    if(An11[i]*An12[i]*An21[i]*An22[i]>10^(-10)  
       & An11[i]<A011[i]
    ){
      LLn1[i] = -2*(n*An11[i]*log(A011[i] / An11[i]) + 
                      n*An12[i]*log(A012[i] / An12[i]) + 
                      n*An21[i]*log(A021[i] / An21[i]) + 
                      n*An22[i]*log(A022[i] / An22[i])) 
    }
  }
  ELn = mean(LLn1)
  EL_100[b] = ELn
  interval.time = (Sys.time()-start.time)/b; 
  if(b%%10==0){
    print(b); 
    print(Sys.time() + (B-b)*interval.time);   
  }
  
}

hist(EL_100)
#save(EL_100,file="EL_100.Rdata"); B=10k; 

quantile(EL_100,c(0.8,0.9,0.95,0.99,0.999))
# n=100 B=10k
#       80%       90%       95%       99%     99.9% 
# 0.5010521 0.7489936 1.0092251 1.6415241 2.4707432  



