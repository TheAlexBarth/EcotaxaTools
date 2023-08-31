# Re-classifying to a broader group:
Copepod_obj <- ecopart_example |> 
  add_zoo(func = names_to, col_name = 'name',
          new_names = c('living', 'not-living', 'Copepoda'),
          suppress_print  = TRUE)

#working with a single zoo_df will return name vector
cast1_copes <- ecopart_example$zoo_files$bats361_ctd1 |> 
  names_to(new_names = c('living', 'not-living', 'Copepoda'))
# how many copepods in cast 1?
cast1_copes |> table()
