# Get depth information
ecoaprt_example |> 
  get_all('depth_including_offset') |> 
  mean()

# Get size range
ecopart_example |> 
  get_all('esd', pixel_conv = T) |> 
  range()