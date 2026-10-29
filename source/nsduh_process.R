#' nsduh_process
#' 
#' Extract and tidy drug use tables from previously imported html
#'
#' @param html the nsduh html
#' @param table_num number of table to extract and tidy
#'
#' @returns cleaned dataframe
#'
nsduh_process = function(html, table_num) {
  
  processed_df = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |>
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  return(processed_df)
  
}