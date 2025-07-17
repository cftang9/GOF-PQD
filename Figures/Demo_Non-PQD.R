rm(list=ls(all=TRUE))
library(MASS)
#source("EL_Library.r")
n = 50; mu = c(0,0); rho=-0.5; 

set.seed(1000000)
V = mvrnorm(n=n,mu=c(0,0),Sigma=array(c(1,rho,rho,1),c(2,2)))
Ind = array(,n)

mean(V[,1]<=0 & V[,2]<=0)
mean(V[,1]<=0 & V[,2]>0)
mean(V[,1]>0 & V[,2]<=0)
mean(V[,1]>0 & V[,2]>0)

for(i in 1:n){
  x = V[i,1]; y = V[i,2]; 

  phi11 = mean(V[,1]<= x& V[,2]<= y); 
  phi12 = mean(V[,1]<= x& V[,2]> y); 
  phi21 = mean(V[,1]> x& V[,2]<= y); 
  phi22 = mean(V[,1]> x& V[,2]> y); 

  phi1d = phi11+phi12; 
  phid1 = phi11+phi21; 

  #Ind[i] = c(phi11<phi1d*phid1); 
}



#pdf("Demo_nonPQD.pdf", width=6, height=6)
par(mar=c(2.1,2.1,0.1,0.1))
plot(V, xlim=c(-3,3), ylim=c(-3,3), 
     frame.plot=FALSE,axes=FALSE)
#points(V[Ind==1,], pch=3)
abline(v=0,lty=1); 
abline(h=0,lty=1); 
text(-3,3,expression(A[12]),cex=1.2)
text(-3,-0.25,expression(A[11]),cex=1.2)
text(3,-0.25,expression(A[21]),cex=1.2)
text(3,3,expression(A[22]),cex=1.2)
text(0.22,-0.18,expression("(x,y)"),cex=1.2)
points(0,0,pch=20,lwd=2)

#Rcpp_GOF_PQD_FF(V,n)

-2*20*(0.10*log(0.21/0.10) + 0.25*log(0.14/0.25) + 0.50*log(0.39/0.50) + 0.15*log(0.26/0.15))

#legend("bottomright", pch=c(1,3), legend=c(expression(phi[11]<=phi[11]^{(0)}),expression(phi[11]>phi[11]^{(0)})))
#dev.off()



