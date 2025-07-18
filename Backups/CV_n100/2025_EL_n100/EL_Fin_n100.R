EL_Fin_100 <- function(Data){
  X = Data[,1]; Y = Data[,2]; 
  n = length(X); 
  Hn = array(,n); 
  An11 = Hn; An12 = Hn; An21 = Hn; An22 = Hn;
  A011 = Hn; A012 = Hn; A021 = Hn; A022 = Hn;
  An011 = Hn; An012 = Hn; An021 = Hn; An022 = Hn;
  LLn1 = array(0,n); 
  for(i in 1:n){
    IFn = c(X<=X[i]); IGn = c(Y<=Y[i]); 
    Hn[i] = mean(IFn*IGn); 
    An11[i] = Hn[i]; 
    An12[i] = mean(IFn) - Hn[i]; 
    An21[i] = mean(IGn) - Hn[i]; 
    An22[i] = 1 - An11[i] - An12[i] - An21[i]; 
    A011[i] = (An11[i]+An12[i])*(An11[i]+An21[i]); 
    A012[i] = (An11[i]+An12[i])*(1-(An11[i]+An21[i])); 
    A021[i] = (1-(An11[i]+An12[i]))*(An11[i]+An21[i]); 
    A022[i] = (1-(An11[i]+An12[i]))*(1-(An11[i]+An21[i])); 
    An011[i] = max(A011[i],An11[i]);
    An012[i] = min(A012[i],An12[i]);
    An021[i] = min(A021[i],An21[i]);
    An022[i] = max(A022[i],An22[i]);
    
    llr11 = 0; llr12 = 0; llr21 = 0; llr22 = 0; 
    
    if(An11[i]>10^(-10) & An11[i]<A011[i]){
      llr11 = n*An11[i]*log(A011[i] / An11[i])
    }
    if(An12[i]>10^(-10) & An11[i]<A011[i]){
      llr12 = n*An12[i]*log(A012[i] / An12[i])
    }
    if(An21[i]>10^(-10) & An11[i]<A011[i]){
      llr21 = n*An21[i]*log(A021[i] / An21[i])
    }
    if(An22[i]>10^(-10) & An11[i]<A011[i]){
      llr22 = n*An22[i]*log(A022[i] / An22[i])
    }
    LLn1[i] = -2*(llr11 + llr12 + llr21 + llr22) 
  }
  ELn = mean(LLn1)
  Reject = c(ELn>1.0092251); 
  return(list(TS=ELn, Rejection=Reject))
}
