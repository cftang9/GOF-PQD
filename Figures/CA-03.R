rm(list=ls(all=TRUE))
library(copula)

value = seq(0.5,10.0,by=0.1); theta=0.6

inv_u = (value/(1-theta))^(-1/theta)
plot(NULL,xlim=c(0,1), ylim=c(0,1),xlab="u",ylab="v")
lines(c(0,1),c(1,0))
#lines(c(0,1),c(0,1))
for(k in 1:length(value)){
  lines(c(1-inv_u[k],1-inv_u[k]),c(inv_u[k],0))
  lines(c(1-inv_u[k],1-0),c(inv_u[k],inv_u[k]))
}
text(0.27,0.05,0.8)
text(0.45,0.05,0.9)
text(0.57,0.05,"1.0")
text(0.66,0.05,"1.1")




dCA_copula = function(u,v,theta=0.3){
  I1 = c(u<v); I2 = c(u>v); I3 = c(u==v);
  dCA_copula = (
      I1 * (1-theta)*v^(-theta)
    + I2 * (1-theta)*u^(-theta)
    + I3 * theta*u^(1-theta)
  )
  return(dCA_copula)
}

Delta = 0.0025
u = seq(Delta,1-Delta,by=Delta); v = u;
nu = length(u); nv = nu;
w = c();
for(i in 1:nu){
  for(j in 1:nv){
    w = c(w,dCA_copula(u[i],1-v[j]))
  }
}
cuv<-t(matrix(w,nrow=nu,byrow=TRUE))

contour(x=u, y=v, z=cuv,
        xlab="u", ylab="v",
        xlim = c(0,1),
        ylim = c(0,1),
        nlevels = 50)


#lines(c(0,0,1,1,0),c(0,1,1,0,0))