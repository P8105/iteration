#' sim_mean_sd
#'
#' @param n_subj number of subjects
#' @param mu true sample mean
#' @param sigma true sample sd
#'
#' @returns tibble with sample mean and sample sd
#' 
#' 
sim_mean_sd = function(n_subj, mu = 3, sigma = 2) {
  
  sim_df = 
    tibble(
      x = rnorm(n = n_subj, mean = mu, sd = sigma)
    )
  
  sim_df |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
  
}