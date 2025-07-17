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
  n = length(Data[,1]); 
  mesh = seq(0,1,by=1/(m+1)); 

  C_n = array(,n); 
  u_n = rank(Data[,1])/(n+1); v_n = rank(Data[,2])/(n+1); 
  
  for(i in 1:n){
    C_n[i] = mean(u_n<=u_n[i] & v_n<=v_n[i]); 
  }
  AD = sum( (pmax(u_n*v_n-C_n,0))^2/(u_n*v_n*(1-u_n)*(1-v_n))); 
  print(AD)
  
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
  ADb = array(0,BB); Reject_BB=0
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
      cu = round(cu,digit=10)
      cu = sort(cu); 
      cu = (cu - cu[1])/(cu[length(mv)]-cu[1]); 
      v[nn] = cu[sum(cu <= v_unif[nn])]
    }
    
    C_nb = array(,n); 
    u_nb = rank(u)/(n+1); v_nb = rank(v)/(n+1); 
    
    for(i in 1:n){
      C_nb[i] = mean(u_nb<=u_nb[i] & v_nb<=v_nb[i]); 
    }
    ADb[bb] = sum( (pmax(u_nb*v_nb-C_nb,0))^2/(u_nb*v_nb*(1-u_nb)*(1-v_nb))); #print(bb)
    
    if(timer==1){
      time_int = (Sys.time()-start_time)/bb
      print(bb)
      print(ADb[bb])
      print(mean(ADb[1:bb]>AD))
      print(time_int)
      print(Reject_BB/bb*BB)
      print(Sys.time()+time_int*(BB-bb))
    }
  }
  
  ADbq = quantile(ADb,0.95); 
  
  return(list(TS = AD, 
              CV = as.numeric(quantile(ADb,0.95)), 
              pvalue = mean(ADb>AD), 
              Rejection = as.numeric(AD>ADbq),
              Reject_BB = Reject_BB))
}



