### Gijbel and Sznajder 2010

#rm(list=ls(all=TRUE))

kernel_Epanechnikov <- function(x,h=1){
  n = length(x); 
  k = array(,n); 
  u = abs(x)/h;
  ind = (u<1); 
  k = ((3/4)*(1-u^2))*ind; 
  return(k)
}

Kernel_Epanechnikov <- function(x,h=1){
  n = length(x); 
  k = array(,n); 
  u = x/h;
  Il = c(u<= -1);
  Ir = c(u>   1);
  Im = c(-1<u & u<=1);
  k = Il * 0 + Ir * 1 + Im * (3*u - u^3 + 2)/4; 
  return(k)
}

a0 <- function(u,h=1){
  l = (u-1)/h; r = u/h
  a0 = 3/4*(
    (r - r^3/3) - (l - l^3/3)
  )
  return(a0)
}

a1 <- function(u,h=1){
  l = (u-1)/h; r = u/h
  a1 = 3/4*(
    (r^2/2 - r^4/4) - (l^2/2 - l^4/4)
  )
  return(a1)
}

a2 <- function(u,h=1){
  l = (u-1)/h; r = u/h; 
  a2 = 3/4*(
    (r^3/3 - r^5/5) - (l^3/3 - l^5/5)
  )
  return(a2)
}

k_uh <- function(x,u,h=1){
  l = (u-1)/h; r = u/h; 
  k_uh = c(l<x & x<r)*(
    kernel_Epanechnikov(x)*(a2(u,h) - a1(u,h)*x)
  )/(a0(u,h)*a2(u,h) - (a1(u,h))^2)
  return(k_uh)
}


K_uh <- function(x,u,h=1){
  l = (u-1)/h; r = u/h; 
  Il = c(x<= l);
  Im = c(l<x & x< r);
  Ir = c(x>= r);
  
  K_uh = 0*Il + 1*Ir + 
    Im*(
    a2(u,h)*(3/4*( (x - x^3/3) - (l - l^3/3) ))
    - a1(u,h)*(3/4*( (x^2/2 - x^3/4) - (l^2/2 - l^4/4) ))
  )/(a0(u,h)*a2(u,h) - (a1(u,h))^2)
  return(K_uh)
}


n = 100; m = 14; 
h1 = 1.5/m; h2 = h1; 
m.I = 19; 

B = 1000; 

GS2010_LL = function(Data,m=14,h1=1.5/m,h2=h1,m.I=19){
  n = length(Data[,1]); 
  mesh = seq(0,1,by=1/(m+1)); 
  
  Fn = rank(Data[,1])/n; Gn = rank(Data[,2])/n; 
  Cn = array(,n);
  un = Fn*n/(n+1); vn = Gn*n/(n+1);
  for(i in 1:n){
    Cn[i] = mean(n*Fn/(n+1)<=un[i] & n*Gn/(n+1)<=vn[i]);
  }
  
  Cm = array(,c(m+2,m+2)); 
  um = mesh; vm = mesh; Dm = Cm; 
  for(i in 1:(m+2)){
    for(j in 1:(m+2)){
      Cm[i,j] = mean(Fn*n/(n+1)<=um[i] & Gn*n/(n+1)<=vm[j])
      Dm[i,j] = max(Cm[i,j], um[i]*vm[j])
      if(um[i]==0|um[i]==1|vm[j]==0|vm[j]==1){
        Dm[i,j] = um[i]*vm[j]; 
      }
    }
  }
  
  mesh.I = seq(0,1,by=1/(m.I+1)); 
  Cm.I = array(,c(m.I+2,m.I+2)); 
  um.I = mesh.I; vm.I = mesh.I; #Dm.I = Cm.I; 
  for(i in 1:(m.I+2)){
    for(j in 1:(m.I+2)){
      Cm.I[i,j] = mean(K_uh((um.I[i]-un)/h1, um.I[i])*
                         (K_uh((vm.I[j]-vn)/h2, vm.I[j])))
    }
  }
  
  AD = 0
  for(i in 2:(m.I+1)){
    for(j in 2:(m.I+1)){
      AD = AD + n*(
        (max(0,um.I[i]*vm.I[j]-Cm.I[i,j]))^2
        /(um.I[i]*vm.I[j]*(1-um.I[i])*(1-vm.I[j]))
        *max(0, Cm.I[i,j] - Cm.I[i-1,j] - Cm.I[i,j-1] + Cm.I[i,j])
      )
    }
  }
  
  Reject = c(AD>11.532879); 
  return(list(TS=AD, Rejection=Reject))
}

