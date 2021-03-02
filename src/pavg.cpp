#include <Rcpp.h>
using namespace Rcpp;

//' Mean per row
//'
//' Returns the average of the numeric values, row per row (similar as pmin)
//'
//' @param df DataFrame. Contains only numerical values
//' @param na_rm, boolean. Accept of not the NAs
//' @return NumericVector or mean values.
//' @export
//'
//' @examples pavg( data.frame(a = c(1,2,3), b = c(2,3,3)), na_rm = TRUE)
// [[Rcpp::export]]
Rcpp::NumericVector pavg(Rcpp::DataFrame df, bool na_rm) {


  // transform to matrix with the R function
  Function asMatrix("as.matrix");
  Rcpp::NumericMatrix df_mat = asMatrix(df);

  // allocate vectors
  Rcpp::NumericVector outputs(df_mat.nrow());

  double reps = 0;
  double total = 0;
  // rank per row
  for(int i=0; i < df_mat.nrow(); ++i){

    // create subrow vector
    reps = 0;
    total = 0;

    for(int j=0; j < df_mat.ncol(); ++j){
      double val = df_mat(i,j);

      if(!ISNA(val)){
        total = total + val;
        reps = reps + 1;
      }else if(!na_rm){
        reps = 1;
        total = NA_REAL;
        break;
      }
    }

    if(total == NA_REAL){
      outputs[i] = NA_REAL;
    }else{
      outputs[i] = total / reps;
    }

  }
  return outputs;


}
