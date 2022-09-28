#!/bin/bash

cd {cfg.icon_input_icbc}

# ---------------------------------
# -- Pre-processing
# ---------------------------------

rm -f {filename}

# -- Convert the GRIB files to NetCDF
cdo -t ecmwf -f nc copy era5_ml_nudging.grib era5_ml_nudging.nc
cdo -t ecmwf -f nc copy era5_surf_nudging.grib era5_surf_nudging.nc

# -- Put all variables in the same file
cdo merge era5_ml_nudging.nc era5_surf_nudging.nc era5_original_nudging.nc

# -- Change variable and coordinates names to be consistent with ICON nomenclature
cdo setpartabn,mypartab,convert era5_original_nudging.nc tmp.nc

# -- Order the variables alphabetically 
ncks tmp.nc data_in.nc
rm tmp.nc era5_surf_nudging.nc era5_ml_nudging.nc era5_original_nudging.nc

# ---------------------------------
# -- Re-mapping
# ---------------------------------

# -- Retrieve the dynamic horizontal grid
cdo -s selgrid,2 {cfg.dynamics_grid_filename_scratch} triangular-grid.nc

# -- Create the weights for remapping ERA5 latlon grid onto the triangular grid
cdo gendis,triangular-grid.nc data_in.nc weights.nc

# -- Remap
cdo -s remapdis,triangular-grid.nc data_in.nc era5_final.nc
rm data_in.nc

# --------------------------------------
# -- Create the LNSP variable
# --------------------------------------

# -- Apply logarithm to surface pressure
cdo expr,'LNPS=ln(PS);' era5_final.nc tmp.nc

# -- Put the new variable LNSP in the original file
ncks -A -v LNPS tmp.nc era5_final.nc
rm tmp.nc

# ---------------------------------
# -- Post-processing
# ---------------------------------

# -- Rename dimensions and order alphabetically
ncrename -h -d cell,ncells era5_final.nc
ncrename -h -d nv,vertices era5_final.nc
ncks era5_final.nc {filename}
rm era5_final.nc


# -- Clean the repository
rm weights.nc
rm triangular-grid.nc

