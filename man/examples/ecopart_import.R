# general use of ecopart import
dir_path = system.file('extdata','ae1917_example-data', 
                       package = 'EcotaxaTools', mustWork = TRUE) #specify file folder

ecp_obj <- ecopart_import(dir_path)

# With the example dataset, we are missing zoofiles for bats361_ctd18
# You always want to know *why* you are missing a file
# However, there are cases where this is normal
# If you wish to only keep casts with zoo_files, use trim_to_zoo = T
ecp_obj <- ecopart_import(dir_path, trim_to_zoo = T)
