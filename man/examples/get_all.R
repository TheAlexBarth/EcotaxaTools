# Get depth information
ecopart_example |> 
  get_all('depth_including_offset') |> 
  mean()

# Get size range
ecopart_example |> 
  get_all('esd', pixel_conv = TRUE) |> 
  range()