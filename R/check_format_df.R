#' Function to check a data frame.
#'
#' What is checked:
#' - The format of the table: data.table, data.frame or tibble?
#' - The names of the columns needed: only letters, integers or "_"
#' - The format of the columns: contains only factors and numeric columns
#' - Missing data per column
#' - Other strange values: "NaN", "NA", or +/- Inf
#'
#' And a summary is created on all variables
#'
#' @param data Dataframe containing the dataset to check
#' @param digits integer, number of digits we will round the numerical variables. Default is 6
#'
#' @return string "Check completed".
#' @export
#'
#' @importFrom crayon
#' @importFrom stringr
#' @importFrom snakecase
#' @importFrom psych
#' @importFrom dplyr
#'
#' @examples check_format_df(iris)
check_format_df <- function(data, digits = 6){

  # Check the inputs
  if(length(digits) != 1 &&
     round(as.numeric(digits),0) != digits){
    stop("The input 'digits' is not of the correct format. Needs an integer.")
  }

  if(!is.data.frame(data)){
    stop("The input 'data' is not of the correct format. Needs a dataset.")
  }


  # bullets
  happy <- "\U2713"
  unhappy <- "\U2718"
  ish <- "\U223F"

  # check the format ----------------------------------------------------------
  if(class(data)[1]=="data.frame"){
    cat(crayon::green(happy,"The dataset is a data.frame\n"))
  }else {
    cat(crayon::blue(ish,"The dataset is a", class(data)[1],"\n"))
  }

  # creation of summary -------------------------------------------------------

  df_summary = psych::describe(data)

  df_summary <- dplyr::rename(df_summary,
                              count = n)
  df_summary <- dplyr::rename(df_summary,
                              column = vars)
  df_summary = data.frame(df_summary)
  df_summary[['column']] = colnames(data)


  # check the names -----------------------------------------------------------
  cols_names <- names(data)
  correct_name_format <- snakecase::to_snake_case(cols_names)
  df_summary[['columns_snake_case']] = correct_name_format

  if(sum(cols_names != correct_name_format)>0 ){
    cat(crayon::red(unhappy, "There are names of columns that are not in accepted format:\n"))
    for(i in (1:ncol(data))[cols_names != correct_name_format]){
      cat(crayon::red(paste0("\t- the column '",cols_names[i],"' should have been ",correct_name_format[i],"\n" )))
    }
    cat("\n")
  }else{
    cat(crayon::green(happy,"All column names have correct format.\n"))
  }



  # check the column format ---------------------------------------------------
  cols_classes <- sapply(X = data,  class)
  df_summary[['data_type']] = cols_classes


  all_classes <- unique(cols_classes)
  accepted_classes <- c("numeric", "integer","logical","factor")
  if(any(! all_classes %in% accepted_classes) ){
    cat(crayon::red(unhappy, "There are columns values that are not numerical, boolean or categorical:\n"))
    for(i in (1:ncol(data))[not_correct_name_format]){
      cat(crayon::red(paste0("\t- the column '",cols_names[i],"'\n" )))
    }
    cat("\n")
  }else{
    cat(crayon::green(happy,"All column have correct type\n"))
  }

  df_summary[['type_cat_or_num']] = ifelse(cols_classes== 'factor',
                                           'Categorical',
                                           'Numerical')


  # check missing data --------------------------------------------------------
  cols_missing <- sapply(X = data,  function(x){sum(is.na(x) |
                                                          is.null(x)|
                                                          is.nan(x) |
                                                          stringr::str_to_upper(x) %in% c("NA","NAN"))})

  df_summary[['missing_rows_pct']] = round(cols_missing/nrow(data)*100,digits)

  if(any(cols_missing>0) ){
    cat(crayon:blue(ish, "There are columns with missing values:\n"))
    for(i in (1:ncol(data))[cols_missing>0]){
      cat(crayon::blue(paste0("\t- the column '",cols_names[i],"' has ",df_summary[['missing_rows_pct']][i],"% missing value\n" )))
    }
    cat("\n")
  }else{
    cat(crayon::green(happy,"All columns are full.\n"))
  }


  # Other values to check in data ---------------------------------------------
  cols_infinite <- sapply(X = data,
                          function(x){sum(is.numeric(x) &
                                            (is.infinite(x) |
                                            stringr::str_to_upper(x) %in% c("INF","INFINITE") ))})

  df_summary[['infinite_rows_pct']] = round(cols_infinite/nrow(data)*100,digits)

  if(any(cols_infinite>0) ){
    cat(crayon:blue(ish, "There are columns with infinite value:\n"))
    for(i in (1:ncol(data))[cols_infinite>0]){
      cat(crayon::blue(paste0("\t- the column '",cols_names[i],"'\n" )))
    }
    cat("\n")
  }else{
    cat(crayon::green(happy,"Not any column contain 'Inf'.\n"))
  }

  # Unique values to check in data ---------------------------------------------
  cols_unique <- sapply(X = data,
                          function(x){length(unique(x))})

  df_summary[['num_levels']] = cols_unique



  return(df_summary)
}


