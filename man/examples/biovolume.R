#calcuating biovolume

zoo_df <- ecopart_example$zoo_files[[1]]
biovol <- biovolume(input = zoo_df,
                    shape = 'sphere',
                    pixel_mm = unique(ecopart_example$meta$acq_pixel))

biovol |> summary()
