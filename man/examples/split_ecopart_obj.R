# splitting an ecopart object

split_objs <- ecopart_example |> split_ecopart_obj(split_by = 'stationid')
gf_casts <- split_objs[['gf']]
