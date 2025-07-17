### Ledwina and Wyłupek (2014)
### Method 2
LW2 = function(Data){
  X = Data[,1]; Y = Data[,2]
  n = length(X); 
  rX = rank(X); rY = rank(Y); 
  ADs = array(NA,n); 
  for(j in 1:n){
    Cn.tilde = mean(rX<=rX[j] & rY<=rY[j]); 
    ADs[j] = sqrt(n)*(Cn.tilde-(rX[j]*rY[j])/(n+1)^2)/sqrt( rX[j]*rY[j]*(n-rX[j])*(n-rY[j])/( (n+1)^4 ) ); 
  }
  LW2 = min(ADs)
  return(list(TS=LW2
              ))
}
