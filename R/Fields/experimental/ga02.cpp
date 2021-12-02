#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

// [[Rcpp::export]]
double penalty_MDL(NumericVector y,NumericVector X, NumericVector cp,
                   double x_min, double x_max, double x_inc) {                  // y   : response
  // x   : predictor with varying coefficient
  
  double m = cp[0];                                       //# m   : number of cpts
  // int m_max = 10;
  int n = X.length();
  double pnt;
  if (m == 0) {
    pnt = 0.0;
  } else {

    NumericVector xi;
    NumericVector foo = cp[Range(1,cp.length())];
    // xi= cbind(x_min-x_inc,foo,x_max+x_inc);   
    xi = cp[Range(1,cp.length())];
    xi.push_front(x_min-x_inc);
    xi.push_back(x_max+x_inc);
    // print(xi);//xi  : cpt locations (xi_1,...,xi_m,x.max+x.inc)
    NumericVector n_r; // n.r : no. of obs in each regime
    int x;
    for (int i : Range(0,m)) {                           //[!CAUTION!] This does not handle missing values!
      //print(i);
      NumericVector temp = y[xi[i] <= X & X < xi[i+1]];
      n_r[i] = temp.length();
    }
    // NumericVector term1 = n_r[Range(1,m+1)];
    NumericVector bar = n_r[Range(1,m-1)];
    // print(n_r);
    pnt = log(m) + m*log(n);// + 0.5*m*sum(log(n_r));
 
  }
  return pnt;                                     // # smaller is better
}

/*** R
penalty_MDL(y,X,cp, x_min, x_max, .02)
*/


