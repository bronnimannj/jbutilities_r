#' class_tree_interaction_finder
#'
#' This code creates interaction features via decision trees when the target is a binary column (0/1).
#'
#' Note: ALL features need to be numericals
#'
#' The penetration index used in the formula is the result of the deviance of a node divided by the sum of case weights for each observation reaching the node.
#'
#' The results of this function is a data frame containing each node, their target penetration, and a R formula to recreate the node from the features.
#'
#' @param  df,               the main frame of data where the target and explanatory variables reside
#' @param  target,           the name of the binary target column
#' @param  n_trees,          number of trees to test. Default: 100
#' @param  feature_fraction, double, between 0 and 1. Will randomly select a subset of features on each tree if feature_fraction is smaller than 1.Default = 1.
#' @param  lower_pt,         double, between 0 and 1. what is the max target penetration required to keep a node as a low penetration node. Default = 0.1
#' @param  upper_pt   ,      double greater than 0, what is the min target penetration index required to keep a node as a high penetration node? Default = 0.1
#' @param  lower_dev,        double greater than 0, the lower threshold for the deviance of the nodes accepted. Default = 0.1
#' @param  suffix_output,    string, suffix to the output columns
#'
#' @return A (possibly empty) data.frame with the list of leaves interactions
#' @export
#'
#' @importFrom rpart
#'
#' @example class_tree_interaction_finder( data.frame("a" = c(0,1,2),"b" = c(2,1,1),"target" = c(0,0,1)), 'target')
#'
class_tree_interaction_finder <- function(df, target, n_trees = 100, feature_fraction = 1,
                                    lower_pt = 0.1, upper_pt = 0.1, lower_dev = 0.1,
                                    suffix_output = 'tree'){

  # Test the inputs
  if(!is.numeric(n_trees)){
    stop("We need n_trees to be numeric")
  }
  if(!is.numeric(feature_fraction)){
    stop("We need feature_fraction to be numeric")
  }else if(feature_fraction <= 0 | feature_fraction > 1){
    stop("We need feature_fraction >0 and <= 1")
  }

  if(!is.numeric(lower_pt)){
    stop("We need lower_pt to be numeric")
  }else if(lower_pt <= 0 | lower_pt > 1){
    stop("We need lower_pt >0 and <= 1")
  }
  if(!is.numeric(upper_pt)){
    stop("We need upper_pt to be numeric")
  }else if(upper_pt <= 0){
    stop("We need upper_pt >0 and <= 1")
  }
  if(!is.numeric(lower_dev)){
    stop("We need lower_dev to be numeric")
  }else if(lower_dev <= 0){
    stop("We need lower_dev >0")
  }
  if (!is.numeric(df[[target]]) | min(df[[target]]) != 0 | max(df[[target]]) != 1){
    stop("Target field not in numerical binary format!")
  }
  for(col in colnames(df)){
    if (!is.numeric(df[[col]])){
      stop(paste("The column '", col, "' is not numerical!"))
    }
  }

  # creates an output data for 'good' tree leaves
  feature_leaves <- data.frame(node_booleans = character(),
                               target_penetration = double(),
                               deviance = double(),
                               total_weight = double())


  # asserts the main dataset
  main_data <- df
  # removes target field from data to avoid random selection
  main_data[target] <- NULL

  # number of features per tree
  number_features = floor(ncol(main_data) * feature_fraction)

  # creates the number of trees specified by the function
  for (i in 1 : n_trees){

    # randomly selects the requested number of fields for this tree
    tree_data <- main_data[ ,
                            sample(x = 1 : ncol(main_data),
                                   size = number_features,
                                   replace = F)]

    tree <- rpart::rpart(
      formula = as.factor(df[[target]]) ~ .,
      data = tree_data,
      method = "class",
      control = rpart::rpart.control(minsplit=30, cp=0.001)
      )

    # stores the tree's output
    tree_frame <- tree$frame
    tree_frame$node_id <- row.names(tree_frame)
    tree_frame$target_penetration <- (tree_frame$dev / tree_frame$wt) / mean(df[[target]])

    # subset the nodes to only include nodes meeting the criteria
    # keeps only terminal nodes with high/low penetration and high deviance
    tree_frame <- subset(tree_frame,
                         ncompete == 0 &
                           (target_penetration < lower_pt |
                              target_penetration > upper_pt) &
                           dev > lower_dev)

    # tidies up the tree frame into final results
    tree_results <- data.frame(node_booleans = as.character(rpart::path.rpart(tree,
                                                                       tree_frame$node_id,
                                                                       print.it = F)),
                               target_penetration = tree_frame$target_penetration,
                               deviance = tree_frame$dev,
                               total_weight = tree_frame$wt,
                               stringsAsFactors = F)

    # binds results from this tree to the main output
    feature_leaves <- rbind(feature_leaves, tree_results)

    # de-duplicate as we go along
    feature_leaves <- feature_leaves[!duplicated(feature_leaves), ]

    # print progress status
    print(paste("Tree #",
                i,
                ", features so far:",
                nrow(feature_leaves)))

  }

  if (nrow(feature_leaves) > 0) { # instances with nodes (albeit possibly single field nodes)

    # order the output with the highest target penetration at the top
    feature_leaves <- feature_leaves[order(feature_leaves$target_penetration,
                                           decreasing = T), ]

    # begin feature function creation
    # remove irrelevant characters from start of script (e.g. root syntax)
    feature_leaves$node_feature <- substr(feature_leaves$node_booleans,
                                          11,
                                          nchar(feature_leaves$node_booleans))
    # remove the slashes, spaces and speech marks added by the package
    feature_leaves$node_feature <- gsub('[\"]',
                                        '',
                                        feature_leaves$node_feature)
    # extract each feature from the tree output
    feature_leaves$node_feature <- lapply(feature_leaves$node_feature,
                                          function(x) paste(paste("df$",
                                                                  unlist(strsplit(x, ",")),
                                                                  sep = ""),
                                                            collapse = " & "))

    feature_leaves$node_feature <- gsub('df[:$:]\\s',
                                        'df$',
                                        feature_leaves$node_feature)
    feature_leaves$node_feature <- gsub('[)]',
                                        '',
                                        feature_leaves$node_feature)

    # convert this to string
    feature_leaves$node_feature <- as.character(feature_leaves$node_feature)

    # wrap the string in an ifelse statement
    feature_leaves$node_feature <- paste("df$",
                                         suffix_output,
                                         "_",
                                         1 : nrow(feature_leaves),
                                         " <- ",
                                         "ifelse(",
                                         feature_leaves$node_feature,
                                         ", 1, 0)",
                                         sep = "")



    # remove one field trees
    # a leaf is based on one feature/field if it contains no &s to state multiple fields
    feature_leaves$multi_field <- grepl('&', feature_leaves$node_feature)
    feature_leaves <- subset(feature_leaves, feature_leaves$multi_field == TRUE)
    feature_leaves$multi_field <- NULL

    # remove the node_booleans as formatted version now exists
    remove_me <- which(feature_leaves$node_booleans == 'root')

    if(length(remove_me) != 0){
      feature_leaves <- feature_leaves[-remove_me,]
    }
    feature_leaves$node_booleans <- NULL
    return(feature_leaves)
  } else { # instance without nodes
    return(
      data.frame( target_penetration = double(),
                  deviance = double(),
                  total_weight = double(),
                  node_feature = character())

    )
  }
}
