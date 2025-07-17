rm(list=ls(all=TRUE))
library(copula)


Delta = 0.01
u = seq(Delta,1-Delta,by=Delta); v = u; 
nu = length(u); nv = nu; 
cop = claytonCopula(iTau(claytonCopula(),-0.2))
w = c(); 
for(i in 1:nu){
  for(j in 1:nv){
    w = c(w,dCopula(c(u[i],v[j]),cop))
  }
}
cuv<-t(matrix(w,nrow=nu,byrow=TRUE))

contour(x=u, y=v, z=cuv,
        xlab="u", ylab="v", 
        xlim = c(0,1),
        ylim = c(0,1),
        nlevels = 100)


#lines(c(0,0,1,1,0),c(0,1,1,0,0))