#' lotr_import
#' 
#' Extract word data from the LotR_Words excel file. 
#'
#' @param cell_range cell range for movie
#' @param movie_title name of movie
#'
#' @returns imported / cleaned dataset
#'
lotr_import = function(cell_range, movie_title) {
  
  lotr_movie_df = 
    read_excel("data/LotR_Words.xlsx", range = cell_range) |> 
    mutate(movie = movie_title) |> 
    janitor::clean_names() |> 
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words"
    ) |> 
    select(movie, everything())
  
  return(lotr_movie_df)
  
}