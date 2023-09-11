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
double IndPqd_el_prod(NumericMatrix X){
  int n = std::size(X(_,0));
  NumericVector Pn11(n*n);
  NumericVector Pn12(n*n);
  NumericVector Pn21(n*n);
  NumericVector Pn22(n*n);
  NumericVector Fn(n*n);
  NumericVector Gn(n*n);
  double Tn=0;
  int ii; int ij;
  for(int i=0; i<n*n; i++){
    ii = i/n; ij = i%n; 
    double x = X(ii,0); double y = X(ij,1); 
    Pn11(i) = mean(ifelse(X(_,0)<=x & X(_,1)<=y,1,0)); 
    Pn12(i) = mean(ifelse(X(_,0)<=x & X(_,1)>y,1,0)); 
    Pn21(i) = mean(ifelse(X(_,0)>x & X(_,1)<=y,1,0)); 
    Pn22(i) = (double)1 - Pn11(i) - Pn12(i) - Pn21(i); 
    Fn(i) = Pn11(i)+Pn12(i); Gn(i) = Pn11(i)+Pn21(i);
    if(Pn11(i)>Fn(i)*Gn(i)){
      if(Pn11(i)*Fn(i)*Gn(i)>0){
        Tn = Tn + n*Pn11(i)*log((Fn(i)*Gn(i)/Pn11(i)));
      }
      if(Pn12(i)*Fn(i)*(1-Gn(i))>0){
        Tn = Tn + n*Pn12(i)*log((Fn(i)*(1-Gn(i)))/Pn12(i));
      }
      if(Pn21(i)*(1-Fn(i))*Gn(i)>0){
        Tn = Tn + n*Pn21(i)*log(((1-Fn(i))*(Gn(i)))/Pn21(i));
      }
      if(Pn22(i)*(1-Fn(i))*(1-Gn(i))>0){
        Tn = Tn + n*Pn22(i)*log(((1-Fn(i))*(1-Gn(i)))/Pn22(i));
      }
    }
  }
  return -2*Tn/(n*n);
}

// [[Rcpp::export]]
double IndPqd_el_joint(NumericMatrix X){
  int n = std::size(X(_,0));
  NumericVector Pn11(n);
  NumericVector Pn12(n);
  NumericVector Pn21(n);
  NumericVector Pn22(n);
  NumericVector Fn(n);
  NumericVector Gn(n);
  double Tn=0;
  for(int i=0; i<n; i++){
    double x = X(i,0); double y = X(i,1); 
    Pn11(i) = mean(ifelse(X(_,0)<=x & X(_,1)<=y,1,0)); 
    Pn12(i) = mean(ifelse(X(_,0)<=x & X(_,1)>y,1,0)); 
    Pn21(i) = mean(ifelse(X(_,0)>x & X(_,1)<=y,1,0)); 
    Pn22(i) = (double)1 - Pn11(i) - Pn12(i) - Pn21(i); 
    Fn(i) = Pn11(i)+Pn12(i); Gn(i) = Pn11(i)+Pn21(i);
    if(Pn11(i)>Fn(i)*Gn(i)){
      if(Pn11(i)*Fn(i)*Gn(i)>0){
        Tn = Tn + n*Pn11(i)*log((Fn(i)*Gn(i)/Pn11(i)));
      }
      if(Pn12(i)*Fn(i)*(1-Gn(i))>0){
        Tn = Tn + n*Pn12(i)*log((Fn(i)*(1-Gn(i)))/Pn12(i));
      }
      if(Pn21(i)*(1-Fn(i))*Gn(i)>0){
        Tn = Tn + n*Pn21(i)*log(((1-Fn(i))*(Gn(i)))/Pn21(i));
      }
      if(Pn22(i)*(1-Fn(i))*(1-Gn(i))>0){
        Tn = Tn + n*Pn22(i)*log(((1-Fn(i))*(1-Gn(i)))/Pn22(i));
      }
    }
  }
  return -2*Tn/n;
}
