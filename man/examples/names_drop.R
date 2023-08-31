# Droping all not-living categories:
ecopart_example <- ecopart_example |> mod_zoo(func = names_drop,
                                              drop_names = 'not-living',
                                              drop_children = TRUE)

# Can be done with a single DF:
# Drop all artefacts
cast1_no_arte <- ecopart_example$zoo_files$bats361_ctd1 |> 
  names_drop('artefact',drop_children = TRUE)