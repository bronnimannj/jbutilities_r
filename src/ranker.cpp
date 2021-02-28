#include <Rcpp.h>
using namespace Rcpp;

//' Rank numerical values
//'
//' Returns the rank of the a numerical column from a dataset of Numerical values
//'
//' @param values NumericVector, containing all the values to rank
//' @param df DataFrame, Data frame of values against we will rank (NA included).
//' @param method String, either "min", "avg" or "max". when there are equalities in the values.
//' @export
//'
//' @examples
//' ranker(c(1,2),data.frame(x = c(3,4),y = c(5,6)), "min")
//' ranker(10,data.frame(x = 10 , y = 10, z = 11, w = 12, u = 13),"min")
// [[Rcpp::export]]
Rcpp::NumericVector ranker(Rcpp::NumericVector values,
                           Rcpp::DataFrame df,
                           Rcpp::String method){

  // transform to matrix with the R function
  Function asMatrix("as.matrix");
  Rcpp::NumericMatrix mdf = asMatrix(df);

  // allocate vectors
  Rcpp::NumericVector rank(values.size());
  Rcpp::NumericVector rowVec(mdf.ncol()+1);

  double reps = 1;

  // rank per row
  for(int i=0; i<values.size(); ++i){

    if(!ISNA(values[i])){
      // create subrow vector
      rowVec[0] = values[i];
      for(int j=1; j<rowVec.size(); ++j){
        double compRP = mdf(i,(j-1));
        if(!ISNA(compRP)){
          rowVec[j] = compRP;
        }else{
          rowVec[j] = R_PosInf;
        }
      }

      // sort row
      std::sort(rowVec.begin(), rowVec.end());

      int iter = std::lower_bound(rowVec.begin(), rowVec.end(), values[i])  - rowVec.begin();

      // find target position in memory with iterator
      if(method == "min"){
        reps = 1;
      }else if(method == "max"){
        reps = 1;
        while (iter + reps < rowVec.size() && values[i] == rowVec[iter + reps] ) {
          ++reps;
        }
      }else if(method == "avg"){
        reps = 1;
        while (iter + reps < rowVec.size() && values[i] == rowVec[iter + reps] ) {
          ++reps;
        }
        reps = (reps + 1) * 0.5;
      }

      rank[i] =  iter + reps;

      // calculate rank
    }else{
      rank[i] = NA_REAL;
    }
  }
  return rank;
}
