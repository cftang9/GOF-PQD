rm(list=ls(all=TRUE))

n = 10000; 

#set.seed(081624000+b)

# Copula A
Data = array(runif(2*n),c(n,2))
Ind = c(Data[,1]>=0.15 & Data[,1]<=0.5 & Data[,2]>=0.5 & Data[,2]<=0.85)
Data[Ind, ] =  cbind( 0.5 - (Data[Ind, 1]-0.15) , 0.5 + (Data[Ind, 1]-0.15))

#Mixture of Frank I
library(copula)
gam = 0.5; 
Ind = rbinom(n,1,prob=gam); 
cop.pos = frankCopula( 9.5); 
cop.neg = frankCopula(-9.5); 
Data = Ind * rCopula(n,cop.pos) + (1-Ind) * rCopula(n,cop.neg)


#Mixture of Frank II 
library(copula)
gam = 0.6; 
Ind = rbinom(n,1,prob=gam); 
cop.pos = frankCopula( 10); 
cop.neg = frankCopula(-15); 
Data = Ind * rCopula(n,cop.pos) + (1-Ind) * rCopula(n,cop.neg)


#Asymmetric copula
Cv_inv = function(u,v,theta_1 = -0.12, theta_2 = 8.3){
  a = theta_1*(-2*v*sin(v*theta_2)); 
  b = theta_1*(1-v^2)*theta_2*cos(v*theta_2); 
  Cv_inv = (1+a+b - sqrt((1+a+b)^2 - 4*(a+b)*u))/(2*(a+b))
  return(Cv_inv)
}
Data = array(runif(2*n),c(n,2))
Data = cbind(Cv_inv(Data[,1],Data[,2]),Data[,2])

# quadratic regression
Data = array(rnorm(2*n),c(n,2))
Data = cbind(Data[,1],0.3*(Data[,1])^2 + Data[,2])


# sin regression
Data = array(rnorm(2*n),c(n,2))
Data = cbind(Data[,1],7*sin(2*pi*Data[,1]) + Data[,2])
plot(Data)

# FGM copula
library(copula)
fgm.cop <- fgmCopula(-1,dim=2)
Data <- rCopula(n, fgm.cop)

# CA copula 
# library(Distributacalcul)
# Data = crBivariateCA(200, dependencyParameter=0.2)
rCA_coupla = function(n=100,theta=0.5){
  u = runif(n); v = runif(n); 
  if(theta == 0){
    rCA_coupla = cbind(u,v)
  }
  if(theta>0 & theta<1){
    I1 = c(v>0 & v< u^(1-theta)*(1-theta)); 
    I2 = c(v>=u^(1-theta)*(1-theta) & v<u^(1-theta)); 
    I3 = c(v>=u^(1-theta) & v<1); 
    Inv = (  I1 * u^(theta)*v/(1-theta)
             + I2 * pmax(u+v-1,0)
             + I3 * v^(1/(1-theta))
    )
    rCA_coupla = cbind(u*(1-I2)+pmax(u+v-1,0)*I2, Inv)
  }
  if(theta == 1){
    w = pmax(u+v-1,0)
    rCA_coupla = cbind(w,w)
  }
  return(rCA_coupla)
}
Data = rCA_coupla(n,theta=0.5)
Data[,1] = 1-Data[,1]; 

# Restricted-t
library(mvtnorm)
Data = rmvt(n,df=1); 
Data = abs(Data)
Data[,1] = -Data[,1]
#plot(Data)
#plot(rank(Data[,1])/(n+1), rank(Data[,2])/(n+1))



