### Gijbel and Sznajder 2013

kernel_Epanechnikov <- function(x,h=1){
  n = length(x); 
  k = array(,n); 
  u = abs(x)/h;
  ind = (u<1); 
  k = ((3/4)*(1-u^2))*ind; 
  return(k)
}

GS2013<- function(Data,m=14,h1=1.5/m,h2=h1,BB=1000,timer=0){
  #library(mvtnorm)
  #set.seed(100); X = rmvnorm(100,sigma = array(c(1,0.9,0.9,1),c(2,2))) ; m=14; h1=1.5/m; h2=h1; BB=1000; timer=0; 
  n = length(Data[,1]); 
  mesh = seq(0,1,by=1/(m+1)); 
  
  Fn = ecdf(Data[,1]); Gn = ecdf(Data[,2]); 
  C_n = array(,n); 
  u_n = Fn(Data[,1])*n/(n+1); v_n = Gn(Data[,2])*n/(n+1); 
  
  for(i in 1:n){
    C_n[i] = mean(u_n<=u_n[i] & v_n<=v_n[i]); 
  }
  #CvM = sum( (pmax(Fn(X[,1])*Gn(X[,2])-C_n,0))^2 );
  AD = sum( (pmax(u_n*v_n-C_n,0))^2/(u_n*v_n*(1-u_n)*(1-v_n))); 
  
  # delta = 0.05; 
  # u_g = seq(0,1,by=delta); v_g = seq(0,1,by=delta);
  # d = length(u_g); 
  # u_g = u_g[-d]; u_g = u_g[-1];
  # v_g = v_g[-d]; v_g = v_g[-1];
  # d = d - 2; 
  # C_g = array(,d^2); 
  # 
  # KS = 0; 
  # for(i in 1:d^2){
  #    ii = (i - 1) %/% d + 1;
  #    ij = (i - 1) %%  d + 1;
  #    C_g[i] = mean(n*Fn(X[,1])/(n+1)<=u_g[ii] & n*Gn(X[,2])/(n+1)<=v_g[ij]); 
  #    KS = max(sqrt(n)*max(pmax(u_g[ii]*v_g[ii]-C_g[i],0)),KS); 
  # }

  mesh = seq(0,1,by=1/(m+1)); 
  ui = array(,(m+2)^2); vj = ui; 
  Cm = array(,(m+2)^2); Dmij = array(,(m+2)^2); 
  for(i in 1:(m+2)^2){
    ii = (i - 1) %/% (m+2) + 1;
    ij = (i - 1) %%  (m+2) + 1;
    ui[i] = mesh[ii]; vj[i] = mesh[ij]; 
    Cm[i] = mean(u_n<=ui[i] & v_n<=vj[i])
    Dmij[i] = max(Cm[i], ui[i]*vj[i])
    if(ui[i]==0|ui[i]==1|vj[i]==0|vj[i]==1){
      Dmij[i] = ui[i]*vj[i]; 
    }
  }  
  
  #KSb = array(0,BB); 
  #CvMb = array(0,BB); 
  ADb = array(0,BB); 
  if(timer==1){
    start_time = Sys.time(); print(start_time)
  }
  for(bb in 1:BB){
    set.seed(061425000+bb)
    u = runif(n); v_unif = runif(n); v = array(,n); 
    for(nn in 1:n){
      ou = u[nn]; 
      mv = seq(0,1,by=1/n); #resolution of the inverse method
      cu = array(0,length(mv)); 
      for(jj in 1:(length(mv))){
        ov = mv[jj]; 
        XWX = array(,c(3,3)); XWY = array(,c(3,1)); 
        XWX[1,1] = sum(1
                       *(kernel_Epanechnikov((ui-ou), h1))
                       *(kernel_Epanechnikov((vj-ov), h2))
                       *(ui-ou)^0
                       *(vj-ov)^0); 
        XWX[1,2] = sum(1
                       *(kernel_Epanechnikov((ui-ou), h1))
                       *(kernel_Epanechnikov((vj-ov), h2))
                       *(ui-ou)^1
                       *(vj-ov)^0); 
        XWX[1,3] = sum(1
                       *(kernel_Epanechnikov((ui-ou), h1))
                       *(kernel_Epanechnikov((vj-ov), h2))
                       *(ui-ou)^0
                       *(vj-ov)^1); 
        XWX[2,1] = XWX[1,2]; 
        XWX[2,2] = sum(1
                       *(kernel_Epanechnikov((ui-ou), h1))
                       *(kernel_Epanechnikov((vj-ov), h2))
                       *(ui-ou)^2
                       *(vj-ov)^0); 
        XWX[2,3] = sum(1
                       *(kernel_Epanechnikov((ui-ou), h1))
                       *(kernel_Epanechnikov((vj-ov), h2))
                       *(ui-ou)^1
                       *(vj-ov)^1); 
        XWX[3,1] = XWX[1,3];
        XWX[3,2] = XWX[2,3];
        XWX[3,3] = sum(1
                       *(kernel_Epanechnikov((ui-ou), h1))
                       *(kernel_Epanechnikov((vj-ov), h2))
                       *(ui-ou)^0
                       *(vj-ov)^2); 
        
        XWY[1,1] = sum(1
                       *(kernel_Epanechnikov((ui-ou), h1))
                       *(kernel_Epanechnikov((vj-ov), h2))
                       *(ui-ou)^0
                       *(vj-ov)^0
                       *Dmij);
        XWY[2,1] = sum(1
                       *(kernel_Epanechnikov((ui-ou), h1))
                       *(kernel_Epanechnikov((vj-ov), h2))
                       *(ui-ou)^1
                       *(vj-ov)^0
                       *Dmij);
        XWY[3,1] = sum(1
                       *(kernel_Epanechnikov((ui-ou), h1))
                       *(kernel_Epanechnikov((vj-ov), h2))
                       *(ui-ou)^0
                       *(vj-ov)^1
                       *Dmij);
          cu[jj] = solve(XWX, XWY)[2];   
      }
      cu = sort(cu); 
      cu = (cu - cu[1])/(cu[length(mv)]-cu[1]); 
      v[nn] = quantile(cu,v_unif[nn]); 
    }
    
    Fnb = ecdf(u); Gnb = ecdf(v); 
    C_nb = array(,n); 
    u_nb = Fnb(u)*n/(n+1); v_nb = Gnb(v)*n/(n+1); 
    
    for(i in 1:n){
      C_nb[i] = mean(u_nb<=u_nb[i] & v_nb<=v_nb[i]); 
    }
    #CvMb[bb] = sum( (pmax(Fnb(u)*Gnb(v)-C_nb,0))^2 );
    ADb[bb] = sum( (pmax(u_nb*v_nb-C_nb,0))^2/(u_nb*v_nb*(1-u_nb)*(1-v_nb))); #print(bb)
    
    
    # C_gb = array(,d^2); 
    # for(i in 1:d^2){
    #   ii = (i - 1) %/% d + 1;
    #   ij = (i - 1) %%  d + 1;
    #   C_gb[i] = mean(n*Fnb(u)/(n+1)<=u_g[ii] & n*Gnb(v)/(n+1)<=v_g[ij]); 
    #   KSb[bb] = max(sqrt(n)*max(pmax(u_g[ii]*v_g[ii]-C_gb[i],0)),KSb[bb]); 
    # }
    
    # if(timer==1){
    #   time_int = (Sys.time()-start_time)/bb
    #   print(bb)
    #   print(time_int)
    #   print(Sys.time()+time_int*(BB-bb))
    # }
  }
  
  #KSbq = quantile(KSb,0.95); 
  #CvMbq = quantile(CvMb,0.95); 
  ADbq = quantile(ADb,0.95); 
  
  return(list(TS = AD, 
              pvalue = mean(ADb>AD), 
              Rejection = as.numeric(AD>ADbq)))
}



