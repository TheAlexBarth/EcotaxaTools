#concentration of a whole projects
zoo_conc <- ecopart_example |> uvp_zoo_conc(breaks = seq(0,1200,100))

# concentration for particular  taxa
zoo_conc <- ecopart_example |>
  add_zoo(func = names_to, col_name = 'name', new_names = c('Chaetognatha','Copepoda','Eumalacostraca','living','not-living'), suppress_print = T) |> # rename for simplicity
  mod_zoo(names_drop, drop_names = 'not-living') |> #remove detritus & artefact
  uvp_zoo_conc(cast_name = c('bats361_ctd1', 'bats361_ctd2'), breaks = seq(0,1200,100)) |> # get concentration
  lapply(bin_format) #formatting

#take a peek:
head(zoo_conc$bats361_ctd1,8)
head(zoo_conc$bats361_ctd2,8)


#volumetric concentraiton for a single cast
cast1_copepod_conc <- ecopart_example |>
  mod_zoo(names_keep, keep_names = 'Copepoda', keep_children = T) |>
  add_zoo(func = names_to, col_name = 'name', new_names = 'Copepoda', suppress_print = T) |>
  add_zoo(func = biovolume, col_name = 'biovol', shape = 'ellipsoid', pixel_mm = unique(ecopart_example$meta$acq_pixel)) |>
  uvp_zoo_conc(cast_name = 'bats361_ctd12', breaks = seq(0,500,100)) |>
  bin_format()

cast1_copepod_conc