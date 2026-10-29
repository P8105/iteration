#' sim_mean_sd
#' 
#' Simulates data and generates a df with the mean and sd
#'
#' @param n_obs sample size
#' @param mu true mean
#' @param sigma true sd
#'
#' @returns dataframe with mean and sd
#'
sim_mean_sd = function(n_obs, mu = 12, sigma = 3) {
  
  sim_df = 
    tibble(
      x = rnorm(n = n_obs, mean = mu, sd = sigma)
    )
  
  return_df = 
    sim_df |> 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
  return(return_df)
  
}
