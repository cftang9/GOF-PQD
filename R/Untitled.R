#osqp::osqp()
#test <- function(){ return(3)}
#?osqp::osqp()

testfunction <- function(X, m = 7){
  n = length(X[,1]); 
  
  dX1 = quantile(X[,1], (1:m)/(m+1)); 
  dX2 = quantile(X[,2], (1:m)/(m+1)); 
  
  Hn = array(,c(m,m)); 
  #Cn = array(,c(m,m)); 
  
  Fn_1 = array(,m); 
  Gn_1 = array(,m); 
  
  for(i in 1:m){
    Fn_1[i] = mean(X[,1]<=dX1[i]); 
    Gn_1[i] = mean(X[,2]<=dX2[i]); 
  }

  d = m^2; 
  
  b = array(,c(3,d)); 
  D_F = array(,d); 
  
  for(i in 1:d){
    d1 = (i-1)%/%m + 1; d2 = (i-1)%%m + 1; 
    Hn[d1,d2] = mean(X[,1]<=dX1[d1] & X[,2]<=dX2[d2]); 
    b[1,i] = 1;
    b[2,i] = -Gn_1[d2];
    b[3,i] = -Fn_1[d1]; 
    D_F[i] = Hn[d1,d2] - Fn_1[d1]*Gn_1[d2]; 
  }
  
  V_F = array(,c(d,d));
  for(i in 1:(d^2)){
    A = array(,c(3,3));
    k = (i-1)%/%d + 1; l = (i-1)%%d + 1;
    k1 = (k-1)%/%m + 1; k2 = (k-1)%%m + 1;
    l1 = (l-1)%/%m + 1; l2 = (l-1)%%m + 1;
    A[1,1] = Hn[min(k1,l1), min(k2,l2)] - Hn[k1, k2]*Hn[l1, l2];
    A[2,1] = Hn[min(l1,k1), l2] - Hn[l1, l2]*Fn_1[k1]; 
    A[3,1] = Hn[l1, min(l2,k2)] - Hn[l1, l2]*Gn_1[k2]; 
    A[1,2] = Hn[min(k1,l1), k2] - Hn[k1, k2]*Fn_1[l1]; 
    A[2,2] = Fn_1[min(k1,l1)] - Fn_1[k1]*Fn_1[l1]; 
    A[3,2] = Hn[min(k1,l1), min(k2,l2)] - Fn_1[l1]*Gn_1[k2]; 
    A[1,3] = Hn[k1,min(k2,l2)] - Hn[k1,k2]*Gn_1[l2]; 
    A[2,3] = Hn[min(k1,l1), min(k2,l2)] - Fn_1[k1]*Gn_1[l2]; 
    A[3,3] = Gn_1[min(k2,l2)] - Gn_1[k2]*Gn_1[l2]; 
    V_F[k,l] = t(b[,k])%*%A%*%b[,l]; 
  }
  
  P = solve(V_F); #print(P)
  q = -P%*%D_F; 
  A_d = diag(d); 
  l = rep(0,d); 
  u = rep(Inf,d); 
  settings <- osqp::osqpSettings(verbose = FALSE)
  model <- osqp::osqp(P=P, q=q, A=A_d, l=l, u=u, par=settings); 
  #print(model$Solve())
  print(model$Solve()$x)
  
  eta_F = n*t(model$Solve()$x-D_F)%*%P%*%((model$Solve()$x-D_F))
  
  #return(list(Hn=Hn,Fn=Fn,Gn=Gn))
  return(eta_F)
}

# w = array(rnorm(800),c(400,2))
# testfunction(w,m=7)
