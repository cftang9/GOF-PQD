#include <Rcpp.h>

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
Rcpp::List GofPqd_Dist(NumericMatrix X){
  int n = std::size(X(_,0));
  NumericVector Fn(n);
  NumericVector Gn(n);
  NumericVector Cn(n);
  
  for(int i=0; i<n; i++){
    Fn(i) = sum(ifelse(X(_,0)<=X(i,0),1,0))/double(1+n); 
    Gn(i) = sum(ifelse(X(_,1)<=X(i,1),1,0))/double(1+n); 
    Cn(i) = sum(ifelse(X(_,0)<=X(i,0) & X(_,1)<=X(i,1),1,0))/double(1+n); 
  }
  double KS = 0; double CvM = 0; double AD = 0;
  for(int i=0; i<n; i++){
    if(Cn(i)>Fn(i)*Gn(i)){
      KS = std::max( pow(n,0.5)*(Cn(i)-Fn(i)*Gn(i)), KS);
      CvM = pow(Cn(i)-Fn(i)*Gn(i),2) + CvM;
      AD = pow(Cn(i)-Fn(i)*Gn(i),2)/(Fn(i)*Gn(i)*(1-Fn(i))*(1-Gn(i))) + AD;
    }
  }
  List TTS = List::create(Named("KS") = KS, Named("CvM") = CvM, Named("AD") = AD); 
  return TTS;
}


