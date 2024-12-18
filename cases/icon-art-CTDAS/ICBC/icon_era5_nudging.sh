#!/bin/bash

cd {ERA5_folder}

module load daint-mc NCO CDO

# ---------------------------------
# -- Pre-processing
# ---------------------------------

rm -f {filename}

# -- Put all variables in the same file
cdo -O merge {era5_ml_file} {era5_surf_file} era5_original.nc

# -- Change variable and coordinates names to be consistent with ICON nomenclature
cdo setpartabn,mypartab,convert era5_original.nc tmp.nc

# -- Order the variables alphabetically 
ncks -O tmp.nc data_in.nc
rm tmp.nc era5_original.nc

# ---------------------------------
# -- Re-mapping
# ---------------------------------

# -- Retrieve the dynamic horizontal grid
cdo -s selgrid,2 {cfg.input_files_scratch_dynamics_grid_filename} triangular-grid.nc

# -- Create the weights for remapping ERA5 latlon grid onto the triangular grid
cdo gendis,triangular-grid.nc data_in.nc weights.nc

# -- Remap
cdo -s remapdis,triangular-grid.nc data_in.nc era5_final.nc
rm data_in.nc

# --------------------------------------
# -- Create the LNSP variable
# --------------------------------------

# -- Apply logarithm to surface pressure
cdo expr,'LNPS=ln(PS); Q=QV; GEOP_SFC=GEOSP' era5_final.nc tmp.nc

# -- Put the new variable LNSP in the original file
ncks -A -v LNPS,Q,GEOP_SFC tmp.nc era5_final.nc
rm tmp.nc

# ---------------------------------
# -- Post-processing
# ---------------------------------

# -- Rename dimensions and order alphabetically
ncrename -h -d cell,ncells era5_final.nc
ncrename -h -d nv,vertices era5_final.nc
ncks -O era5_final.nc {filename}
rm era5_final.nc


# -- Clean the repository
rm weights.nc
rm triangular-grid.nc

