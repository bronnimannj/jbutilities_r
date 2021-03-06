#' Creation of a random dataset
#'
#' A function to create a dataset containing :
#' - a ID column containing ID from 1 to the size
#' - normally distributed variables (if NULL, no column)
#' - factors variables with the levels given by list input (if NULL, no column)
#' - Booleans with binomial distributions with list input (if NULL, no column)
#'
#'
#'
#' @param size Integer, number of rows of the final dataset.
#' @param seed Numeric. If NA, a seed is randomly creacted. Else, takes the seed given.
#' @param Numerics Character vector of names of normally distributed numeric vectors
#' @param Booleans Named vector of probabilities between 0 and 1
#' @param Categories List of factor variables, each element containing the levels as character vector
#'
#' @return data.frame that can be tested by check_format_df
#' @export
#'
#' @importFrom snakecase to_any_case
#'
#' @examples create_df_rd(10,NA, c("num1","num2"), c(0.2,0.3),list(a = c(1,2,3),b = c("F","M")))
create_df_rd <- function(size = 10,
                         seed = as.numeric(NA),
                         Numerics = NULL,
                         Booleans = NULL,
                         Categories = NULL){

  # check inputs format --------------------------------------------------------
  if(length(size) != 1 &&
     round(as.numeric(size),0) != size){
    stop("The input 'size' is not of the correct format. Needs an integer.")
  }

  if(is.na(seed)){
    seed = as.integer((as.double(Sys.time()) * 1000 + Sys.getpid()) %% 2^31)
  }else if(! (length(seed) == 1 &&
              abs(as.integer(as.numeric(seed),0) - as.integer(seed)) < 10^{-6})){
    stop("The input 'seed' is not of the correct format. Needs an integer.")
  }

  if(!is.null(Numerics) &&
     !( length(Numerics) >= 1 &&
        class(Numerics) == "character") ){
    stop("The input 'Numerics' is not of the correct format. Needs a string vector.")
  }

  if(!is.null(Booleans) &&
     !( length(Booleans) >= 1 &&
        class(Booleans) == "numeric" &&
        max(Booleans) <= 1 &&
        min(Booleans) >= 0) ){
    stop("The input 'Booleans' is not of the correct format. Needs a probabilities vector.")
  }

  if(!is.null(Categories) &&
     !( length(Categories) >= 1 &&
        class(Categories) == "list" &&
        all(sapply(Categories,class) %in% c("numeric","logical","character"))) ){
    stop("The input 'Categories' is not of the correct format. Needs a list of vectors containing the levels.")
  }

  # check names of the columns--------------------------------------------------

  # Numerics
  if(!is.null(Numerics)){
    Numerics <- snakecase::to_any_case(Numerics,case = "snake")
  }

  # Bools
  if(!is.null(Booleans)){
    names(Booleans) <- snakecase::to_any_case(names(Booleans),
                                              case = "snake")

    for(i in 1:length(Booleans)){
      if(is.na(names(Booleans)[i]) | names(Booleans)[i]=="" ){
        names(Booleans)[i] <- paste0("Bool_",i)
      }
    }
  }

  # Categories
  if(!is.null(Categories)){
    names(Categories) <- snakecase::to_any_case(names(Categories),
                                                case = "snake")
    for(i in 1:length(Categories)){
      if(is.na(names(Categories)[i]) | names(Categories)[i]==""){
        names(Categories)[i] <- paste0("Cat_",i)
      }
    }
  }

  # Creation of the dataset ----------------------------------------------------

  # ID
  data <- data.frame( ID = seq(1,size,1),
                      stringsAsFactors = FALSE)

  # add the numeric columns
  if(!is.null(Numerics)){
    for(i in 1: length(Numerics)){
      set.seed(seed+i)
      data[[Numerics[i]]] <- stats::rnorm(size,0,1)
    }
  }

  # add the booleans
  if(!is.null(Booleans) && !is.null(names(Booleans))){
    for(i in 1: length(Booleans)){
      set.seed(seed+i)
      data[[names(Booleans[i])]] <- as.logical(stats::rbinom(size,1,Booleans[i]))
    }
  }


  # add the categories
  if(!is.null(Categories)&& !is.null(names(Categories))){
    for(i in 1: length(Categories)){
      set.seed(seed+i)
      data[[names(Categories)[i]]] <- factor(sample(Categories[[i]],
                                                    size,
                                                    replace = TRUE),
                                             levels = Categories[[i]])
    }
  }

  # return the data
  data
}

