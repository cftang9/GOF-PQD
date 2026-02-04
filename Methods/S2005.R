#Scaillet 2005 Guassian Multiplier Method
#KS-type Copula-based GOF test

PQD.Dist.Joint.KS <- function(Data){
  X = Data[,1]; Y = Data[,2]; 
  n = length(X); 
  #calculate the empirical copula
  Fn = rank(X)/n; Gn = rank(Y)/n; 
  Ui = Fn; Vi = Gn; 
  
  KS = 0; 
  
  delta = 0.05; 
  mesh = seq(0,1,by=delta); mesh = mesh[mesh>0 & mesh<1]; 
  m = length(mesh); 
  dXm = quantile(X,mesh); dYm = quantile(Y,mesh)
  Hm = array(,c(m,m)); H1m = Hm; H2m = Hm; 
  Cm = array(,c(m,m)); C1m = Cm; C2m = Cm; 
  C11m = Cm; C12m = Cm; C21m = Cm; C22m = Cm; 
  
  for(i in 1:m^2){
    ii = (i-1)%/% m + 1; 
    ij = (i-1)%% m + 1; 
    IC1 = c(Ui<=mesh[ii]); IC2 = c(Vi<=mesh[ij])
    Cm[ii,ij] = mean(IC1*IC2); 
    C1m[ii,ij] = mesh[ii]; C2m[ii,ij] = mesh[ij]; 
  }
  
  #calculate the KS test statisitcs
  KS = sqrt(n)*max(C1m*C2m-Cm); 
  
  h1m = 1.05*n^(-1/5)*sd(X); 
  h2m = 1.05*n^(-1/5)*sd(Y); 
  dX1m = as.numeric(quantile(X, (1:m)/(m+1),type=1)); 
  dX2m = as.numeric(quantile(Y, (1:m)/(m+1),type=1)); 
  dHdX1m = array(,c(m,m)); dHdX2m = array(,c(m,m)); 
  fX1m = array(,c(m,m)); fX2m = array(,c(m,m)); 
  Cr_1m = array(,c(m,m)); Cr_2m = array(,c(m,m)); 
  for(i in 1:m^2){
    ii = (i-1)%/% m + 1; 
    ij = (i-1)%% m + 1;
    dHdX1m[ii,ij] = (n*h1m)^(-1)*sum(dnorm((dX1m[ii]-X)/h1m)*pnorm((dX2m[ij]-Y)/h2m));
    dHdX2m[ii,ij] = (n*h2m)^(-1)*sum(dnorm((dX2m[ij]-Y)/h2m)*pnorm((dX1m[ii]-X)/h1m));
    fX1m[ii,ij] = (n*h1m)^(-1)*sum(dnorm((dX1m[ii]-X)/h1m));
    fX2m[ii,ij] = (n*h2m)^(-1)*sum(dnorm((dX2m[ij]-Y)/h2m));
    Cr_1m[ii,ij] = dHdX1m[ii,ij]/fX1m[ii,ij];
    Cr_2m[ii,ij] = dHdX2m[ii,ij]/fX2m[ii,ij];
  }

  # Multiplier Bootstraps
  
  BB = 1000;
  KSmg = array(,BB); 
  Fn_E = ecdf(X); Gn_E = ecdf(Y); 
  for(bb in 1:BB){
    set.seed(081724000+bb)
    Z = rnorm(n); 
    
    GCm = array(0,c(m,m)); 
    GHm = array(0,c(m,m)); 
    for(i in 1:m^2){
      ii = (i-1)%/%m + 1; 
      ij = (i-1)%%m + 1;
      ICm1 = c(Ui<=mesh[ii]); 
      ICm2 = c(Vi<=mesh[ij]); 
      UICm = Z*(ICm1*ICm2 - Cm[ii,ij])/sqrt(n)
      UIIm = Z*(Cr_1m[ii,ij]*(ICm1 - mesh[ii]) + Cr_2m[ii,ij]*(ICm2 - mesh[ij]))/sqrt(n)
      GCm[ii,ij] = sum(UIIm-UICm); 
    }
    KSmg[bb] = max(GCm); 
  }
  CV = as.numeric(quantile(KSmg,0.95)); 
  
  return(list(TS = KS, 
              CV = CV,
              pvalue = mean(KSmg>KS),
              Rejection = as.numeric(KS>CV)
  )); 
}
