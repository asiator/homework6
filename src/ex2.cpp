#include <Rcpp.h>
#include <vector>
#include <list>
#include <utility>
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]
//' @title simplify2array
//' @description This function filled  matrix by vectors column-by-column.
//'
//' @param x - list of numeric vectors
//' @return matrix 
//'
//' @export
//[[Rcpp::export]]
RObject  simplify2array(List x){
  if(Rf_isNull(x))stop("x is null");
  if(x.size()==0)stop("x is empty");
  for ( auto xi : x ){
    if(!Rf_isNumeric(xi))stop("vector is not numeric");   
  }
  bool diff_size=false;
  int size=0;
  for ( NumericVector xi : x ){
    for ( NumericVector xii : x ){
      if(xi.size()!=xii.size()){
        diff_size=true;
     }
    }
    size=xi.size();
  }
  NumericMatrix out(size,x.length());
  if(diff_size){
    return x;
  }else{
    
    int i=0,j=0;
    for ( NumericVector xi : x ){
      for ( auto xii : xi){
        out(j,i)=xii;
        j++;
      }
      j=0;
      i++;
    }
    return out;
  }
 
}


/*** R
library(testthat)
expect_error(home6::simplify2array(list(c("a"), c("b"))))
expect_error(home6::simplify2array(list()))
expect_error(home6::simplify2array(data.frame))
expect_is(home6::simplify2array(list(c(1,2), c(1,2))), "matrix")
expect_is(home6::simplify2array(list(c(1,2), c(1,2,3))), "list")
expect_equal(home6::simplify2array(list(c(1,2), c(1,2,3))), base::simplify2array(list(c(1,2), c(1,2,3))))
expect_equal(home6::simplify2array(list(c(1,2), c(1,2))), base::simplify2array(list(c(1,2), c(1,2))))
expect_equal(home6::simplify2array(list(c(1,NA,2), c(1,2))), base::simplify2array(list(c(1,NA,2), c(1,2))))
expect_equal(home6::simplify2array(list(c(1,2,NULL), c(1,2))), base::simplify2array(list(c(1,2,NULL), c(1,2))))
expect_equal(home6::simplify2array(list(c(1,2,3), c(1,2,8))), base::simplify2array(list(c(1,2,3), c(1,2,8))))
expect_equal(home6::simplify2array(list(c(1,2,3), c(1,2,NA))), base::simplify2array(list(c(1,2,3), c(1,2,NA))))
*/

