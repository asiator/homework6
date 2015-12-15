# include <Rcpp.h>
# include <vector>
using namespace Rcpp;
//[[Rcpp::plugins("cpp11")]]
//' @title mode
//' @description This function determine the most frequently occurring value in an integer vector (mode). If the mode is ambiguous (e.g. for 1, 2, 2, 2, 3, 3, 1, 3), return any mode
//'
//' @param v - integer vector
//' @return integer value
//'
//' @export
// [[Rcpp::export]]
int mode(IntegerVector v){
  if(Rf_isNull(v))stop("v is null");
  int count=0;
  for ( int i =0; i < v.size (); i++ ){
    if(IntegerVector::is_na(v[i])){count++;}
  }
  if(count==v.size())stop("only NA");
  std::vector <std::pair<int,int> > freq;
  freq.push_back(std::make_pair(v[0],0));
  bool bylo=false;
  for ( int i =0; i < v.size (); i++ ){
    for (int j =0; j<freq.size(); j++  ) {
      if(!IntegerVector::is_na(v[i])){
        if(v[i]==freq[j].first){
          freq[j].second++;
          bylo=true;
        } 
      }else{
        bylo=true;
      }
    }
    if(!bylo){
      freq.push_back(std::make_pair(v[i], 1));
    }
    bylo=false;
  }
  
  std::pair<int,int> max=std::make_pair(0,0);
  for ( auto fi : freq){
    if(max.second<fi.second){
      max.first=fi.first;
      max.second=fi.second;
    }
  }
  return max.first;
}


/*** R
library(testthat)
expect_error(home6::mode())
expect_error(home6::mode(c("a")))
expect_error(home6::mode(list(c(1,2))))
expect_error(home6::mode(data.frame()))
expect_error(home6::mode(c()))
expect_error(home6::mode(c(NA)))
expect_is(home6::mode(c(1,2)),"integer")
expect_equal(home6::mode(c(1,1,1,2,2,2,3)), 1)
expect_equal(home6::mode(c(1,1,1,2,2,2,2,3)), 2)
expect_equal(home6::mode(c(1,1,1,2,NA,NA,NULL,2,2,2,3)), 2)
expect_equal(home6::mode(c(1,1,1,2,2,2,2,3,NA, 3, 8,1,4,1)), 1)
expect_equal(home6::mode(c(1,2,NA,NA,NA)), 1)
expect_equal(home6::mode(c(NA,1,2,NA,NA,NA)), 1)
*/
