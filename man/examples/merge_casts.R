#merging casts based on station:
name_map = list(
  'gf' = ecopart_example$meta$profileid[which(ecopart_example$meta$stationid == 'gf')],
  'other' = ecopart_example$meta$profileid[which(ecopart_example$meta$stationid != 'gf')]
)

merged_obj <- ecopart_example |> merge_casts(name_map = name_map)
# now there's only two aggregate casts:
names(merged_obj$par_files)
names(merged_obj$zoo_files)