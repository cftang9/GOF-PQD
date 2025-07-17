rm(list=ls(all=TRUE))
library(copula)
library(mvtnorm)

Delta = 0.01
#pdf("All_6_Figures.pdf", width=7, height=11)

par(mfcol=c(3,2))
par(mar=c(3.6,3.6,3,0.1))

if(1==1){
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
        xlab="", ylab="", 
        xlim = c(0,1),
        ylim = c(0,1),
        nlevels = 50,
        main=expression(paste("Clayton, ",tau," = -0.2")), 
        cex.axis=1.2, cex.main=1.3
        )
mtext("u", side=1, line=2)
mtext("v", side=2, line=2)
}

if(1==1){
  u = seq(Delta,1-Delta,by=Delta); v = u; 
  nu = length(u); nv = nu; 
  cop = frankCopula(iTau(frankCopula(),-0.2))
  w = c(); 
  for(i in 1:nu){
    for(j in 1:nv){
      w = c(w,dCopula(c(u[i],v[j]),cop))
    }
  }
  cuv<-t(matrix(w,nrow=nu,byrow=TRUE))
  
  contour(x=u, y=v, z=cuv,
          xlab="", ylab="", 
          xlim = c(0,1),
          ylim = c(0,1),
          nlevels = 20, 
          main=expression(paste("Frank, ",tau," = -0.2")), 
          cex.axis=1.2, cex.main=1.3
          )
  mtext("u", side=1, line=2)
  mtext("v", side=2, line=2)
}

if(1==1){
  u = seq(Delta,1-Delta,by=Delta); v = u; 
  nu = length(u); nv = nu; 
  cop = gumbelCopula(iTau(gumbelCopula(),0.2))
  w = c(); 
  for(i in 1:nu){
    for(j in 1:nv){
      w = c(w,dCopula(c(u[i],1-v[j]),cop))
    }
  }
  cuv<-t(matrix(w,nrow=nu,byrow=TRUE))
  
  contour(x=u, y=v, z=cuv,
          xlab="", ylab="", 
          xlim = c(0,1),
          ylim = c(0,1),
          nlevels = 100, 
          main=expression(paste("Gumbel, ",tau," = -0.2")), 
          cex.axis=1.2, cex.main=1.3
          )
  mtext("u", side=1, line=2)
  mtext("v", side=2, line=2)
}

if(1==1){
  u = seq(Delta,1-Delta,by=Delta); v = u; 
  nu = length(u); nv = nu; 
  cop = fgmCopula(-0.6)
  w = c(); 
  for(i in 1:nu){
    for(j in 1:nv){
      w = c(w,dCopula(c(u[i],v[j]),cop))
    }
  }
  cuv<-t(matrix(w,nrow=nu,byrow=TRUE))
  
  contour(x=u, y=v, z=cuv,
          xlab="", ylab="", 
          xlim = c(0,1),
          ylim = c(0,1),
          nlevels = 20, 
          main=expression(paste("FGM, ",theta," = -0.6")), 
          cex.axis=1.2, cex.main=1.3
          )
  mtext("u", side=1, line=2)
  mtext("v", side=2, line=2)
}

if(1==1){
  dCA_copula = function(u,v,theta=0.3){
    I1 = c(u<v); I2 = c(u>v); I3 = c(u==v);
    dCA_copula = (
      I1 * (1-theta)*v^(-theta)
      + I2 * (1-theta)*u^(-theta)
      + I3 * theta*u^(1-theta)
    )
    return(dCA_copula)
  }
  
  Delta_s = 0.25*Delta
  u = seq(Delta_s,1-Delta_s,by=Delta_s); v = u;
  nu = length(u); nv = nu;
  w = c();
  for(i in 1:nu){
    for(j in 1:nv){
      w = c(w,dCA_copula(u[i],1-v[j]))
    }
  }
  cuv<-t(matrix(w,nrow=nu,byrow=TRUE))
  
  contour(x=u, y=v, z=cuv,
          xlab="", ylab="",
          xlim = c(0,1),
          ylim = c(0,1),
          nlevels = 50, 
          main=expression(paste("Cuadras-Augé, ",theta," = -0.3")), 
          cex.axis=1.2, cex.main=1.3
          )
  mtext("u", side=1, line=2)
  mtext("v", side=2, line=2)
}

if(1==1){
  Nu = 3
  u = seq(Delta,1-Delta,by=Delta); v = u; 
  nu = length(u); nv = nu; 
  w = c(); 
  for(i in 1:nu){
    for(j in 1:nv){
      w = c(w,dmvt(c(qt(u[i]/2+0.5,df=Nu),qt((1-v[j])/2+0.5,df=Nu)), 
                   delta = rep(0, 2), sigma = diag(2), df = Nu, log = F)/
              (dt(qt(u[i]/2+0.5,df=Nu),df=Nu)
               *dt(qt((1-v[j])/2+0.5,df=Nu),df=Nu))
      )
    }
  }
  cuv<-t(matrix(w,nrow=nu,byrow=TRUE))
  
  contour(x=u, y=v, z=cuv,
          xlab="", ylab="", 
          xlim = c(0,1),
          ylim = c(0,1),
          nlevels = 100, 
          main=expression(paste("Restricted t, ", nu," = 3")), 
          cex.axis=1.2, cex.main=1.3
          )
  mtext("u", side=1, line=2)
  mtext("v", side=2, line=2)
}

par(mfrow=c(1,1))
#dev.off()