#Denuit and Scaillet 2004

# #Distribution-based GOF test
# DS_F <- function(X, m = 7){
#   n = length(X[,1]); 
#   
#   dX1 = as.numeric(quantile(X[,1], (1:m)/(m+1))); 
#   dX2 = as.numeric(quantile(X[,2], (1:m)/(m+1))); 
#   
#   Hn = array(,c(m,m)); 
#   Fn_1 = array(,m); 
#   Gn_1 = array(,m); 
#   
#   for(i in 1:m){
#     Fn_1[i] = mean(X[,1]<=dX1[i]); 
#     Gn_1[i] = mean(X[,2]<=dX2[i]); 
#   }
#   
#   d = m^2; 
#   
#   D_F = array(,d); 
#   for(i in 1:d){
#     i1 = (i-1)%/%m + 1; i2 = (i-1)%%m + 1; 
#     Hn[i1,i2] = mean(X[,1]<=dX1[i1] & X[,2]<=dX2[i2]); 
#     D_F[i] = Hn[i1,i2] - Fn_1[i1]*Gn_1[i2]; 
#   }
#   
#   V_F = array(,c(d,d));
#   for(i in 1:(d^2)){
#     k = (i-1)%/%d + 1; l = (i-1)%%d + 1;
#     k1 = (k-1)%/%m + 1; k2 = (k-1)%%m + 1;
#     l1 = (l-1)%/%m + 1; l2 = (l-1)%%m + 1;
#     V_F[k,l] = (  Fn_1[k1]*
#                     (  Fn_1[l1]* (Gn_1[min(k2,l2)] - Gn_1[k2]*Gn_1[l2])
#                        + Gn_1[l2]* (Hn[l1,k2] - Gn_1[k2]*Fn_1[l1])
#                        -           (Hn[l1,min(k2,l2)] - Gn_1[k2]*Hn[l1,l2]))
#                   + Gn_1[k2]*
#                     (  Fn_1[l1]* (Hn[k1,l2] - Fn_1[k1]*Gn_1[l2])
#                        + Gn_1[l2]* (Fn_1[min(k1,l1)] - Fn_1[k1]*Fn_1[l1])
#                        -           (Hn[min(k1,l1),l2] - Fn_1[k1]*Hn[l1,l2]))
#                   - 1*
#                     (  Fn_1[l1]* (Hn[k1,min(k2,l2)]-Hn[k1,k2]*Gn_1[l2])
#                        + Gn_1[l2]* (Hn[min(k1,l1),k2] - Hn[k1,k2]*Fn_1[l1])
#                        -           (Hn[min(k1,l1),min(k2,l2)] - Hn[k1,k2]*Hn[l1,l2]))
#     )
#   }
#   
#   Singular.Condition = is.singular.matrix(V_F)
#   if(Singular.Condition==F){
#     P = solve(V_F);
#     q = -P%*%D_F;
#     A_d = diag(d);
#     l = rep(0,d);
#     u = rep(Inf,d);
#     settings <- osqp::osqpSettings(verbose = FALSE); 
#     model <- osqp::osqp(P=P, q=q, A=A_d, l=l, u=u, par=settings);
#     
#     eta_F = n*t(model$Solve()$x-D_F)%*%P%*%((model$Solve()$x-D_F)); 
#     
#     BB = 1000; 
#     wddi = array(0,(d+1));
#     for(bb in 1:BB){
#       simu_DF = MASS::mvrnorm(n=1,mu=rep(0,d),Sigma = V_F)
#       simu_model <- osqp::osqp(P=P, q=-P%*%simu_DF, A=A_d, l=l, u=u, par=settings);
#       simu_eta_F = n*t(simu_model$Solve()$x-simu_DF)%*%P%*%((model$Solve()$x-simu_DF)); 
#       simu_Opt_DF = round(simu_model$Solve()$x,6); 
#       simu_ind = sum(simu_Opt_DF<=0)+1; #d-i positive elements, i starts from 0; 
#       wddi[simu_ind] = wddi[simu_ind] + 1/BB; 
#     }
#     pvalue = 0
#     for(j in 1:d){
#       pvalue = pvalue + pchisq(eta_F, df = j, lower.tail=FALSE)*wddi[j]; 
#     }
#     return(list(TestStatistic = eta_F, p_value = pvalue, Reject=c(pvalue<0.05), singular_V_F = Singular.Condition));
#   }
#   if(Singular.Condition==T){
#     return(list(TestStatistic = NA, p_value = NA, Reject=NA, singular_V_F = Singular.Condition));
#   }
# }

#Copula-based GOF test
DS_C <- function(Data, m = 5){
  #Data = array(runif(200),c(100,2))
  X = Data[,1]; Y = Data[,2]
  dX1 = as.numeric(quantile(X, (1:m)/(m+1)));
  dX2 = as.numeric(quantile(Y, (1:m)/(m+1)));
  
  Cn = array(,c(m,m));
  Fn_1 = array(,m);
  Gn_1 = array(,m);
  for(i in 1:m){
    Fn_1[i] = mean(X<=dX1[i]);
    Gn_1[i] = mean(Y<=dX2[i]);
  }
  
  d = m^2;
  D_C = array(,d);
  for(i in 1:d){
    i1 = (i-1)%/%m + 1; i2 = (i-1)%%m + 1;
    Cn[i1,i2] = mean(X<=dX1[i1] & Y<=dX2[i2]);
    D_C[i] = Cn[i1,i2] - Fn_1[i1]*Gn_1[i2];
  }
  
  # estimating density by Gaussian kernel.
  h1 = 1.05*n^(-1/5)*sd(X);
  h2 = 1.05*n^(-1/5)*sd(Y);
  dHdX1 = array(,c(m,m)); dHdX2 = array(,c(m,m));
  fX1 = array(,c(m,m)); fX2 = array(,c(m,m));
  Hr_1 = array(,c(m,m)); Hr_2 = array(,c(m,m));
  for(i in 1:d){
    i1 = (i-1)%/%m + 1; i2 = (i-1)%%m + 1;
    dHdX1[i1,i2] = (n*h1)^(-1)*sum(dnorm((dX1[i1]-X)/h1)*pnorm((dX2[i2]-Y)/h2));
    dHdX2[i1,i2] = (n*h2)^(-1)*sum(dnorm((dX2[i2]-Y)/h2)*pnorm((dX1[i1]-X)/h1));
    fX1[i1] = (n*h1)^(-1)*sum(dnorm((dX1[i1]-X)/h1));
    fX2[i2] = (n*h2)^(-1)*sum(dnorm((dX2[i2]-Y)/h2));
    Hr_1[i1,i2]= dHdX1[i1,i2]/fX1[i1];
    Hr_2[i1,i2]= dHdX2[i1,i2]/fX2[i2];
  }
  V_C = array(,c(d,d));
  for(i in 1:(d^2)){
    k = (i-1)%/%d + 1; l = (i-1)%%d + 1;
    k1 = (k-1)%/%m + 1; k2 = (k-1)%%m + 1;
    l1 = (l-1)%/%m + 1; l2 = (l-1)%%m + 1;
    V_C[k,l] = (  Hr_1[k1,k2]*
                    (  Hr_1[l1,l2]* (Gn_1[min(k2,l2)] - Gn_1[k2]*Gn_1[l2])
                       + Hr_2[l1,l2]* (Cn[l1,k2] - Gn_1[k2]*Fn_1[l1])
                       -           (Cn[l1,min(k2,l2)] - Gn_1[k2]*Cn[l1,l2]))
                  + Hr_2[k1,k2]*
                    (  Hr_1[l1,l2]* (Cn[k1,l2] - Fn_1[k1]*Gn_1[l2])
                       + Hr_2[l1,l2]* (Fn_1[min(k1,l1)] - Fn_1[k1]*Fn_1[l1])
                       -           (Cn[min(k1,l1),l2] - Fn_1[k1]*Cn[l1,l2]))
                  - 1*
                    (  Hr_1[l1,l2]* (Cn[k1,min(k2,l2)] - Cn[k1,k2]*Gn_1[l2])
                       + Hr_2[l1,l2]* (Cn[min(k1,l1),k2] - Cn[k1,k2]*Fn_1[l1])
                       -           (Cn[min(k1,l1),min(k2,l2)] - Cn[k1,k2]*Cn[l1,l2]))
    )
  }
  
  Singular.Condition = c(min(Re(eigen(V_C)$values))<10^(-10) )
  if(Singular.Condition==F){
    P = solve(V_C);
    q = -P%*%D_C;
    A_d = diag(d);
    l = rep(0,d);
    u = rep(Inf,d);
    settings <- osqp::osqpSettings(verbose = FALSE);
    model <- osqp::osqp(P=P, q=q, A=A_d, l=l, u=u, par=settings);
    
    eta_C = n*t(model$Solve()$x-D_C)%*%P%*%((model$Solve()$x-D_C));
    
    BB = 1000; 
    wddi = array(0,(d+1));
    for(bb in 1:BB){
      simu_DC = MASS::mvrnorm(n=1,mu=rep(0,d),Sigma = V_C)
      simu_model <- osqp::osqp(P=P, q=-P%*%simu_DC, A=A_d, l=l, u=u, par=settings);
      simu_eta_C = n*t(simu_model$Solve()$x-simu_DC)%*%P%*%((model$Solve()$x-simu_DC)); 
      simu_Opt_DC = round(simu_model$Solve()$x,6); 
      simu_ind = sum(simu_Opt_DC<=0)+1; #d-i positive elements, i starts from 0; 
      wddi[simu_ind] = wddi[simu_ind] + 1/BB; 
    }
    pvalue = 0
    for(j in 1:d){
      pvalue = pvalue + pchisq(eta_C, df = j, lower.tail=FALSE)*wddi[j]; 
    }
    return(list(TestStatistic = eta_C, p_value = pvalue, Reject=c(pvalue<0.05), singular_V_F = Singular.Condition));
  }
  if(Singular.Condition==T){
    return(list(TestStatistic = NA, p_value = NA, Reject=NA, singular_V_F = Singular.Condition));
  }
}

#rho = 0.0; w = MASS::mvrnorm(n=400, mu=c(0,0), Sigma = array(c(1,rho,rho,1),c(2,2))); DS_C(w,m=7); DS_F(w,m=7);

# p_F = 0; p_C = 0; B=1000;
# start.time = Sys.time();
# set.seed(20240702);
# for(b in 1:B){
#   #X = array(runif(400),c(200,2));
#   rho = -0; X = MASS::mvrnorm(n=400, mu=c(0,0), Sigma = array(c(1,rho,rho,1),c(2,2)))
#   p_F = p_F+as.numeric(DS_F(X,m=7)$p_value<0.05)/B;
#   p_C = p_C+as.numeric(DS_C(X,m=7)$p_value<0.05)/B;
#   interval.time = (Sys.time()-start.time)/b;
#   print(b)
#   print(c(p_F,p_C)*B/b)
#   print(Sys.time()+interval.time*(B-b))
# }
# #0.048 0.036 under independent cases.
