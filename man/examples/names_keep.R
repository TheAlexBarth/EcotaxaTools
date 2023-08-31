# keep only copepods:
copes_only <- ecopart_example |> mod_zoo(names_keep, keep_names = 'Copepoda',
                                         keep_children = TRUE)

# Keep multiple labels:
rhiz <- ecopart_example |> mod_zoo(names_keep, 
                                   keep_names = c('Aulosphaeridae', 
                                                  'Aulacanthidae'),
                                   keep_children = TRUE)

# with a single zoo_df
cope_cast1_df <- ecopart_example$zoo_files$bats361_ctd1 |> 
  names_keep(keep_names = 'Copepoda',keep_children = TRUE)
