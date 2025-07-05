PQD.Dist.Joint.KS <- function(Data){
  X = Data[,1]; Y = Data[,2]; 
  n = length(X); 
  
  Fn = rank(X)/n; Gn = rank(Y)/n; 
  C_n = array(,n); 
  u_n = Fn*n/(n+1); v_n = Gn*n/(n+1); 
  for(i in 1:n){
    C_n[i] = mean(n*Fn/(n+1)<=u_n[i] & n*Gn/(n+1)<=v_n[i]); 
  }

  KS = 0; 
  
  delta = 0.05
  mesh = seq(0,1,by=delta); mesh = mesh[mesh>0 & mesh<1]; 
  m = length(mesh); 
  C_m = array(,m^2); 
  F_m = array(,m^2); 
  G_m = array(,m^2); 
  
  for(i in 1:m^2){
    ii = (i-1)%%  m + 1; 
    ij = (i-1)%/% m + 1; 
    IF = c(Fn*n/(n+1)<=mesh[ii]); IG = c(Gn*n/(n+1)<=mesh[ij])
    C_m[i] = mean(IF*IG)
    F_m[i] = mean(IF)
    G_m[i] = mean(IG)
  }
  
  KS = sqrt(n)*max(pmax(F_m*G_m-C_m,0)); 
  
  return(list(TS = KS, Rejection = c(KS>0.675)
  )); 
}
