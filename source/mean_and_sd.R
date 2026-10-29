#' mean_and_sd
#' 
#' This function computes the mean and sd of an input vector
#'
#' @param x input numeric vector
#'
#' @returns dataframe with mean and sd
#'
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input x must be numeric")
  } else if (length(x) < 5) {
    stop("x should have 5 or more numbers")
  }
  
  return_df = 
    tibble(
      mean = mean(x),
      sd = sd(x)
    )
  
  return(return_df)
  
}