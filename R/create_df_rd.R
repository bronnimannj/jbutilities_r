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
#' @param size Integer, number of profiles to create.
#' @param seed Numeric. Seed for reproducibility. If NULL, a seed is randomly created. The default is NA
#' @param numerics List. Normally distributed numeric columns. ID are the names and values are either NULL or a vector of length 2 with mean and std.
#' @param booleans List. Binomial distributed columns. Keys are the names, values are either NULL or a float which is the probability of True values.
#' @param categories List. Multinomial distributed columns. Keys are the names, values are either NULL or a vector of categorical values. The probabilities will be 1/N if a column has N unique values
#'
#' @return data.frame that can be tested by check_format_df
#' @export
#'
#' @importFrom snakecase to_any_case
#'
#' @examples create_df_rd(10,NULL, list("num1" = NULL,"num2" = c(0,1)), list("bool1" = 0.5),list(a = c(1,2,3),b = c("F","M")))
create_df_rd <- function(size = 10,
                         seed = NULL,
                         numerics = NULL,
                         booleans = NULL,
                         categories = NULL){

  # check inputs format --------------------------------------------------------
  if(length(size) != 1 &&
     round(as.numeric(size),0) != size){
    stop("The input 'size' is not of the correct format. Needs an integer.")
  }

  if(is.null(seed)){
    seed = as.integer((as.double(Sys.time()) * 1000 + Sys.getpid()) %% 2^31)
    print(paste("Seed is NA, we set it to",seed))
  }else if(! (length(seed) == 1 &&
              abs(as.integer(as.numeric(seed),0) - as.integer(seed)) < 10^{-6})){
    stop("The input 'seed' is not of the correct format. Needs an integer.")
  }

  if (is.null(numerics)){
    print("'numerics' is None. We do not have any numerical columns")
  }else if (is.list(numerics)){
    print(paste("We have",length(numerics),"numerical columns"))
  }else{
    stop("The input 'numerics' is not of the correct format.")
  }

  if (is.null(booleans)){
    print("'booleans' is None. We do not have any boolean columns")
  }else if (is.list(booleans)){
    print(paste("We have",length(booleans),"boolean columns"))
  }else{
    stop("The input 'booleans' is not of the correct format.")
  }

  if(!is.null(categories) &&
     !( length(categories) >= 1 &&
        class(categories) == "list" &&
        all(sapply(categories,class) %in% c("numeric","logical","character"))) ){
    stop("The input 'categories' is not of the correct format. Needs a list of vectors containing the levels.")
  }

  # check names of the columns--------------------------------------------------

  # numerics
  if(!is.null(numerics)){
    names(numerics) <- snakecase::to_any_case(names(numerics),case = "snake")

    for(i in 1:length(numerics)){
      if(is.na(names(numerics)[i]) | names(numerics)[i]=="" ){
        names(numerics)[i] <- paste0("num_",i)
      }
    }
  }

  # Bools
  if(!is.null(booleans)){
    names(booleans) <- snakecase::to_any_case(names(booleans),
                                              case = "snake")

    for(i in 1:length(booleans)){
      if(is.na(names(booleans)[i]) | names(booleans)[i]=="" ){
        names(booleans)[i] <- paste0("bool_",i)
      }
    }
  }

  # categories
  if(!is.null(categories)){
    names(categories) <- snakecase::to_any_case(names(categories),
                                                case = "snake")
    for(i in 1:length(categories)){
      if(is.na(names(categories)[i]) | names(categories)[i]==""){
        names(categories)[i] <- paste0("cat_",i)
      }
    }
  }

  # Creation of the dataset ----------------------------------------------------

  # ID
  data <- data.frame( id = seq(1,size,1),
                      stringsAsFactors = FALSE)

  # add the numeric columns
  seed_diff = 0
  if(!is.null(numerics)){
    for(i in 1: length(numerics)){
      set.seed(seed+seed_diff)
      if(is.null(numerics[[i]]) ){
        data[[names(numerics)[i]]] <- stats::rnorm(size,0,1)
      }else{
        data[[names(numerics)[i]]] <- stats::rnorm(size,
                                                   numerics[[i]][1],
                                                   numerics[[i]][2])
      }
      seed_diff = seed_diff + 1
    }
  }

  # add the booleans
  if(!is.null(booleans)){
    for(i in 1: length(booleans)){
      set.seed(seed+seed_diff)
      if(is.null(booleans[[i]]) ){
        data[[names(booleans)[i]]] <- as.logical(stats::rbinom(size,1,0.5))
      }else{
        data[[names(booleans)[i]]] <- as.logical(stats::rbinom(size,
                                                               1,
                                                               booleans[[i]]))
      }
      seed_diff = seed_diff + 1
    }
  }


  # add the categories
  if(!is.null(categories)){
    for(i in 1: length(categories)){
      set.seed(seed+seed_diff)
      if(is.null(categories[[i]]) ){
      } else{
        data[[names(categories)[i]]] <- factor(sample(categories[[i]],
                                                      size,
                                                      replace = TRUE),
                                               levels = categories[[i]])
      }
    }
  }

  # return the data
  data
}

