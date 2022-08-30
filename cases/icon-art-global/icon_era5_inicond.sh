#!/bin/bash

cd {cfg.icon_input_icbc}

# ---------------------------------
# -- Pre-processing
# ---------------------------------

rm -f {cfg.inicond_filename_scratch}

# -- Convert the GRIB files to NetCDF
cdo -t ecmwf -f nc copy era5_ml.grib era5_ml.nc
cdo -t ecmwf -f nc copy era5_surf.grib era5_surf.nc

# -- Put all variables in the same file
cdo merge era5_ml.nc era5_surf.nc era5_original.nc

# -- Apply logarithm to surface pressure
cdo expr,'LNSP=ln(SP);' era5_original.nc tmp.nc

# -- Put the new variable LNSP in the original file
ncks -A -v LNSP tmp.nc era5_original.nc
rm tmp.nc

# -- Change variable and coordinates names to be consistent with ICON nomenclature
cdo setpartabn,mypartab,convert era5_original.nc tmp.nc

# -- Order the variables alphabetically 
ncks tmp.nc data_in.nc
rm tmp.nc era5_original.nc era5_surf.nc era5_ml.nc

# ---------------------------------
# -- Re-mapping
# ---------------------------------

# -- Retrieve the dynamic horizontal grid
cdo -s selgrid,2 {cfg.dynamics_grid_filename_scratch} triangular-grid.nc

# -- Create the weights for remapping ERA5 latlon grid onto the triangular grid
cdo gendis,triangular-grid.nc data_in.nc weights.nc

# -- Extract the land-sea mask variable in input and output files
cdo selname,LSM data_in.nc LSM_in.nc
ncrename -h -v LSM,FR_LAND LSM_in.nc
cdo selname,FR_LAND {cfg.extpar_filename_scratch} LSM_out_tmp.nc

# -- Add time dimension to LSM_out.nc
ncecat -O -u time LSM_out_tmp.nc LSM_out_tmp.nc
ncks -h -A -v time LSM_in.nc LSM_out_tmp.nc

# -- Create two different files for land- and sea-mask
cdo -L setctomiss,0. -ltc,0.5  LSM_in.nc oceanmask_in.nc
cdo -L setctomiss,0. -gec,0.5 LSM_in.nc landmask_in.nc
cdo -L setctomiss,0. -ltc,0.5 LSM_out_tmp.nc oceanmask_out.nc
cdo -L setctomiss,0. -gec,0.5 LSM_out_tmp.nc landmask_out.nc
cdo setrtoc2,0.5,1.0,1,0 LSM_out_tmp.nc LSM_out.nc
rm LSM_in.nc LSM_out_tmp.nc

# -- Set missing values (ocean for land variables) to a distance-weighted average
cdo setmisstodis -selname,SMIL1,SMIL2,SMIL3,SMIL4,STL1,STL2,STL3,STL4,W_SNOW,T_SNOW data_in.nc dataland_in.nc

# -- Remap land variables
cdo remap,triangular-grid.nc,weights.nc dataland_in.nc dataland_out.nc
rm dataland_in.nc

# -----------------------------------------------------------------------------
# -- Remap land and ocean area differently for variables defined on both (SKT)
# -----------------------------------------------------------------------------

# -- Ocean part
# -----------------

# -- Select SKT variable
cdo selname,SKT data_in.nc dataskt_in.nc

# -- Apply the ocean mask to SKT (by dividing)
cdo div dataskt_in.nc oceanmask_in.nc tmp1.nc

# -- Set missing values (land values then) to a distance-weighted average
cdo setmisstodis tmp1.nc tmp2.nc

# -- Remap 
cdo remapdis,triangular-grid.nc tmp2.nc tmp3.nc

# -- Apply the ocean mask to remapped SKT (by dividing)
cdo div tmp3.nc oceanmask_out.nc dataskt_ocean_out.nc

# -- Clean the repository
rm tmp*.nc oceanmask*.nc

# # -- Land part
# # -----------------

cdo div dataskt_in.nc landmask_in.nc tmp1.nc
cdo setmisstodis tmp1.nc tmp2.nc
cdo remapdis,triangular-grid.nc tmp2.nc tmp3.nc
cdo div tmp3.nc landmask_out.nc dataskt_land_out.nc
rm tmp*.nc landmask*.nc dataskt_in.nc

# -- merge remapped land and ocean part
# --------------------------------------

cdo ifthenelse LSM_out.nc dataskt_land_out.nc dataskt_ocean_out.nc dataskt_out.nc
rm dataskt_ocean_out.nc dataskt_land_out.nc

# remap the rest and merge all files
# --------------------------------------

# -- Select all variables apart from these ones
ncks -h -x -v STL1,STL2,STL3,STL4,SMIL1,SMIL2,SMIL3,SMIL4,W_SNOW,T_SNOW,LSM data_in.nc datarest_in.nc

# -- Remap
cdo -s remapdis,triangular-grid.nc datarest_in.nc era5_final.nc
rm data_in.nc datarest_in.nc

# -- Merge remapped files plus land sea mask from EXTPAR
ncks -h -A dataland_out.nc era5_final.nc
ncks -h -A -v SKT dataskt_out.nc era5_final.nc
ncks -h -A -v FR_LAND LSM_out.nc era5_final.nc
ncrename -h -v FR_LAND,LSM era5_final.nc
rm dataland_out.nc

# -- Rename dimensions and order alphabetically
ncrename -h -d cell,ncells era5_final.nc
ncrename -h -d nv,vertices era5_final.nc
ncks era5_final.nc {cfg.inicond_filename_scratch} 
rm era5_final.nc

# ---------------------------------
# -- Post-processing
# ---------------------------------

# -- Clean the repository
rm weights.nc
rm triangular-grid.nc

