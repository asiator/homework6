# include <Rcpp.h>
# include <vector>
# include <list>
# include <utility>
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]
//' @title ass
//' @description This funciotn for some given integer n generates all possible 0-1 assignment vectors of 2n survey participants in such a way that exactly n of them are assigned to group 0 (control) and the other n ones are assigned to group 1 (treatment)
//'
//' @param n - integer value
//' @return matrix with 2n columns and an appropriate number of rows
//'
//' @export
//[[Rcpp::export]]
NumericMatrix ass(int n){
  if(n<=0)stop("n<=0");
  int size=Rcpp::internal::factorial(2*n)/(pow(Rcpp::internal::factorial(n),2));
  NumericMatrix out(size,2*n);
  IntegerVector x(2*n);
  int counter=0;
  bool diff=true;
  for(int i=0; i <n; i++){
    x[i]=1;
  }
  while(diff){
    for( int i=0; i<x.size();i++){
      out(counter,i)=x[i];
    }
    counter++;
    std::next_permutation(x.begin(), x.end());
    diff=false;
    for( int i=0; i<x.size();i++){
      if(!(out(0,i)==x[i]))diff=true;
    }
  }
  return out;
  
}

/*** R
library(testthat)
expect_error(home6::ass())
expect_error(home6::ass("a"))
expect_error(home6::ass(NA))
expect_error(home6::ass(NULL))
expect_error(home6::ass(c(1,2)))
expect_equal(home6::ass(2.5),home6::ass(2))
expect_is(home6::ass(2), "matrix")
expect_equal(length(home6::ass(2)[1,]),4)
expect_equal(length(home6::ass(2)[,1]),6)
expect_equal(length(home6::ass(4)[1,]),8)
expect_equal(length(home6::ass(4)[,1]),70)
*/


