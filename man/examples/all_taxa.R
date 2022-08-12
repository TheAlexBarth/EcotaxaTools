#getting all taxa
ecopart_example |> all_taxa()

#get unique taxa names
ecopart_example |>
  all_taxa() |>
  unique()

#get proportions
ecopart_example |> 
  all_taxa() |> 
  table() |> 
  prop.table()
