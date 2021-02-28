#' Function to check a data frame.
#'
#' What is checked:
#' - The format of the table: data.table, data.frame or tibble?
#' - The names of the columns needed: only letters, integers or "_"
#' - The format of the columns: contains only factors and numeric columns
#' - Missing data per column
#' - Other strange values: "NaN", "NA", or +/- Inf
#'
#' And a summary is created on all numeric variables
#'
#' @param data Dataframe containing the dataset to check
#' @param digits integer, number of digits we will round the numerical variables. Default is 6
#'
#' @return string "Check completed".
#' @export
#'
#' @import crayon
#' @importFrom stringr str_length
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

  # check the names -----------------------------------------------------------
  cols_names <- names(data)
  not_correct_name_format <- !stringr::str_detect(cols_names,"^[a-zA-Z0-9_]*$")
  if(sum(not_correct_name_format)>0 ){
    cat(crayon::red(unhappy, "There are names of columns that contain something else than letters, numerics or underscore:\n"))
    for(i in (1:ncol(data))[not_correct_name_format]){
      cat(crayon::red(paste0("\t- the column '",cols_names[i],"'\n" )))
    }
    cat("\n")
  }else{
    cat(crayon::green(happy,"All column names have correct format.\n"))
  }

  # check the column format ---------------------------------------------------
  cols_classes <- sapply(X = data,  class)

  all_classes <- unique(cols_classes)
  accepted_classes <- c("numeric", "integer","logical","factor")
  if(any(! all_classes %in% accepted_classes) ){
    cat(crayon::red(unhappy, "There are columns values that are not numerical, boolean or categorical:\n"))
    for(i in (1:ncol(data))[not_correct_name_format]){
      cat(crayon::red(paste0("\t- the column '",cols_names[i],"'\n" )))
    }
    cat("\n")
  }else{
    cat(crayon::green(happy,"All column values have correct format.\n"))
  }


  # check missing data --------------------------------------------------------
  cols_missing <- sapply(X = data,  function(x){sum(is.na(x) |
                                                          is.null(x)|
                                                          is.nan(x) |
                                                          stringr::str_to_upper(x) %in% c("NA","NAN"))})

  if(any(cols_missing>0) ){
    cat(crayon:blue(ish, "There are columns with missing value:\n"))
    for(i in (1:ncol(data))[cols_missing>0]){
      cat(crayon::blue(paste0("\t- the column '",cols_names[i],"'\n" )))
    }
    cat("\n")
  }else{
    cat(crayon::green(happy,"All columns are full.\n"))
  }


  # Other values to check in data ---------------------------------------------
  cols_infinite <- sapply(X = data,
                          function(x){sum(is.infinite(x) |
                                            stringr::str_to_upper(x) %in% c("INF","INFINITE") )})

  if(any(cols_infinite>0) ){
    cat(crayon:blue(ish, "There are columns with infinite value:\n"))
    for(i in (1:ncol(data))[cols_infinite>0]){
      cat(crayon::blue(paste0("\t- the column '",cols_names[i],"'\n" )))
    }
    cat("\n")
  }else{
    cat(crayon::green(happy,"Not any column contain 'Inf'.\n"))
  }


  # print summary boolean cols ------------------------------------------------

  booleans <- cols_classes %in% c("logical")
  if(sum(booleans)>0){
    cat("\n")
    cat(crayon::magenta(happy,"Summary of boolean variables:\n"))
    for(i in (1:ncol(data))[booleans]){
      cat(crayon::magenta(paste0("\t- the column '",cols_names[i],"' has ",
                                 sum(data[,i])," True and ",
                                 sum(!data[,i])," False.\n" )))
    }
  }

  # print summary categorical cols --------------------------------------------

  categoricals <- cols_classes %in% c("factor")
  if(sum(categoricals)>0){
    cat("\n")
    cat(crayon::magenta(happy,"Summary of categorical variables:\n"))
    for(i in (1:ncol(data))[categoricals]){
      lvl <- levels(data[,i])

      cat(crayon::magenta(paste0("\t- the column '",cols_names[i],"' has ",
                                 length(lvl)," categories: ",
                                 stringr::str_flatten(lvl[1:(min(length(lvl),
                                                                 3))],
                                                      collapse = " ; "),
                                 if(length(lvl)>3){" ; ..."},".\n" )))
    }
  }

  # print summary numerical cols-----------------------------------------------
  numerical <- cols_classes %in% c("numeric","integer")
  if(sum(numerical)>0){
    cat(crayon::magenta(happy,"Summary of numerical variables:\n"))

    summary_df <- data.frame(variable = c(as.character(NA)),
                             min=c(as.numeric(NA)),
                             Q01=c(as.numeric(NA)),
                             Q10=c(as.numeric(NA)),
                             Q25=c(as.numeric(NA)),
                             mean = c(as.numeric(NA)),
                             median = c(as.numeric(NA)),
                             Q75=c(as.numeric(NA)),
                             Q90=c(as.numeric(NA)),
                             Q99=c(as.numeric(NA)),
                             max = c(as.numeric(NA)),
                             NAs = c(as.numeric(NA)),
                             stringsAsFactors = FALSE)

    for(x in names(cols_classes[numerical])){
      summary_df[nrow(summary_df)+1,"variable"] <- x
      summary_df[nrow(summary_df),"min"] <- round(min(data[[x]],na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"mean"] <- round(mean(data[[x]],na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"median"] <- round(stats::median(data[[x]],na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"max"] <- round(max(data[[x]],na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"Q01"] <- round(stats::quantile(data[[x]],0.01,na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"Q10"] <- round(stats::quantile(data[[x]],0.1,na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"Q25"] <- round(stats::quantile(data[[x]],0.25,na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"Q75"] <- round(stats::quantile(data[[x]],0.75,na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"Q90"] <- round(stats::quantile(data[[x]],0.90,na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"Q99"] <- round(stats::quantile(data[[x]],0.99,na.rm=TRUE),digits)
      summary_df[nrow(summary_df),"NAs"] <- sum(is.na(data[[x]]))
    }
    summary_df <- summary_df[-1,]

    for(i in 1:ncol(summary_df)){
      summary_df[,i] <- as.character(summary_df[,i])
      summary_df[is.na(summary_df[,i]),i] <- "NA"
    }

    df_lens  <- c(max(stringr::str_length(summary_df$variable),9,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$min),4,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$Q01),4,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$Q10),4,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$Q25),4,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$mean),5,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$median),7,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$Q75),4,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$Q90),4,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$Q99),4,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$max),4,na.rm=TRUE),
                                  max(stringr::str_length(summary_df$NAs),4,na.rm=TRUE)) + 3

    spaces <- function(string, lentotal){
      if(is.na(string)){string <- ""}
      paste0(string, paste(rep(" ",
                               lentotal - stringr::str_length(as.character(string))),
                           collapse="")  )
    }
    format_title <- function(x){
      crayon::bgMagenta(crayon::white(x))
    }

    cat(paste0("\t",
               format_title(paste0(spaces("Variable", df_lens[1]),
                                   spaces("Min", df_lens[2]),
                                   spaces("Q01",  df_lens[3]),
                                   spaces("Q10", df_lens[4]),
                                   spaces("Q25", df_lens[5]),
                                   spaces("mean", df_lens[6]),
                                   spaces("median", df_lens[7]),
                                   spaces("Q75", df_lens[8]),
                                   spaces("Q90", df_lens[9]),
                                   spaces("Q99", df_lens[10]),
                                   spaces("max", df_lens[11]),
                                   spaces("NAs", df_lens[12]),"\n"))))
    for(i in 1:nrow(summary_df)){
      txtrow <- ""
      for(j in 1:12){
        txtrow <- paste0(txtrow,
                         spaces(summary_df[i,j], df_lens[j]))
      }
      cat(crayon::magenta(paste0("\t",txtrow,"\n")))
    }

  }



  return("Check completed")
}


