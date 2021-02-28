#include <Rcpp.h>
using namespace Rcpp;

//' Rank A numerical dataset row by row
//'
//' Returns a data frame where each row is ordered. The columns names are "TOP" + the number.
//'
//' @param df DataFrame, Data frame numerical values (NA included).
//' @export
//'
//' @examples
//' rankdf(data.frame(x = c(3,4), y = c(5,6)))
// [[Rcpp::export]]
Rcpp::DataFrame rankdf(Rcpp::DataFrame df){

  // transform to matrix with the R function
  Function asMatrix("as.matrix");
  Rcpp::NumericMatrix matrix_df = asMatrix(df);

  // allocate vectors
  Rcpp::NumericVector rowVec(matrix_df.ncol());

  // rank per row
  for(int i=0; i<matrix_df.nrow(); ++i){

		// create subrow vector
		for(int j=0; j<rowVec.size(); ++j){
			double compRP = matrix_df(i,j);
			if(!ISNA(compRP)){
				rowVec[j] = compRP;
			}else{
				rowVec[j] = R_PosInf;
			}
		}

		// sort row
		std::sort(rowVec.begin(), rowVec.end());

		for(int j=0; j<rowVec.size(); ++j){
			matrix_df(i,j) = rowVec[j];
		}
  }

	//name the columns
	Rcpp::CharacterVector names(rowVec);

	std::string name = "TOP";
	char numstr[21]; // enough to hold all numbers up to 64-bits

	for(int i=0; i<rowVec.size(); ++i){
		sprintf(numstr, "%d", (i+1));
		names[i] = name + numstr;
	}

	Rcpp::DataFrame result(matrix_df);
  result.attr("names") = names;
  return result;
}
