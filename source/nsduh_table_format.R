nsduh_table_format = function(html, table_num, table_name) {
  
  out_table = 
    html |> 
    html_table() |> 
    nth(table_num) |> 
    slice(-1) |> 
    mutate(drug = table_name) |> 
    select(-contains("P Value"))
  
  return(out_table)
  
}