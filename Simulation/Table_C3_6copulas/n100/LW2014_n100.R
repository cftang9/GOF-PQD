LW2_n100 = function(Data){
  Y = Data[,2]
  X = Data[,1]
  n = length(X); 
  rX = rank(X); rY = rank(Y); 
  ADs = array(NA,n); 
  for(j in 1:n){
    Cn.tilde = mean(rX<=rX[j] & rY<=rY[j]); 
    ADs[j] = sqrt(n)*(Cn.tilde-(rX[j]*rY[j])/(n+1)^2)/sqrt( rX[j]*rY[j]*(n-rX[j])*(n-rY[j])/( (n+1)^4 ) ); 
  }
  LW2 = min(ADs)
  return(list(TS=LW2,Rejection=c(LW2< -2.842301)))
}
