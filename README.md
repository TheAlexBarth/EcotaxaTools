# EcotaxaTools
A variety of tools to process and quickly analyze data from Ecotaxa & Ecopart
This package is under heavy development. For a tutorial on it's use check out [my tutorial](TheAlexBarth.Github.io/Ecotaxa_Tools_Tutorial.html)
For updates to the package check out the [updates page](https://thealexbarth.github.io/Ecotaxa_Tools_Tutorial/info_updates-page.html)

If there are any issues, contact Alex Barth abarth1014@gmail.com

## Understanding the package files

### app
 - app_multimanager.R: This contains the multi_manager() function and support files

### archived
 - Primarily Functions to be phased out but kept in for some old code which might require these formats
 - binByTools.R: the old bin_by() and bin_by_df() functions
 - tsvTrim.R: a trimming function to limit tsv columns
 - vol_sampled: a function to calculate UVP volume sampled in bins from original project files

### ecopart
 - add_zoo.R: A function to add a column with function to all zoo_files in an ecopart object list
 - import.R: Importing ecopart files from a directory
 - vol_bin.R: get volume in bins from UVP data ***NEED TO UPDATE***

### gen: files generalizable to any tools
 - bin-helpers.R: functions for helping deal with bin_taxa data
 - bin-taxa.R: Get summary by taxa ID in user-specified depth bins
 - helper-functions.R: general helper functions; add_range(), nearest(), timeOfDay() ***NEED TO UPDATE***, get_col_name()
 - integration-tools.R: linear interpolation and integration between depth bins
 - obj-volume.R: Get the volume of objects in an ecotaxa-style tibble. Two options sph_vol(); ellps_vol()
 - read-etx.R: import ecotaxa .tsv files
 - rename.R: a creative way to rename taxa

### UVP: files for dealing with UVP-specific data
 - conc.R: get concentration of a given cast
