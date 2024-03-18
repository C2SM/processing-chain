#!/bin/bash

cd {cfg.icon_input_icbc}

# ---------------------------------
# -- Pre-processing
# ---------------------------------

rm -f {cfg.icon_input_icbc}/{cfg.meteo_prefix}{cfg.startdate_sim_yyyymmddhh}_ic{cfg.inidata_filename_suffix}

# -- Convert the GRIB files to NetCDF
cdo -t ecmwf -f nc copy {cfg.meteo_dir}/{cfg.meteo_prefix}{cfg.startdate_sim_yyyymmddhh}{cfg.meteo_suffix} era5_ori.nc

# -- Change variable and coordinates names to be consistent with ICON nomenclature
cdo setpartabn,partab_meteo,convert era5_ori.nc tmp.nc

# -- Order the variables alphabetically 
ncks tmp.nc data_in.nc
rm tmp.nc era5_ori.nc

# ---------------------------------
# -- Re-mapping
# ---------------------------------

# -- Retrieve the triangular grid
cdo -s selgrid,2 {cfg.input_files_scratch_dynamics_grid_filename} triangular-grid.nc

# -- Create the weights for remapping era5 latlon grid onto the triangular grid
cdo gendis,triangular-grid.nc data_in.nc weights.nc

# -- Extract the land-sea mask variable 
cdo selname,LSM data_in.nc LSM_in.nc
ncrename -h -v LSM,FR_LAND LSM_in.nc
cdo selname,FR_LAND {cfg.input_files_scratch_extpar_filename} LSM_out_tmp.nc

# -- Add time dimension to LSM_out.nc
ncecat -O -u time LSM_out_tmp.nc LSM_out_tmp.nc
ncks -h -A -v time LSM_in.nc LSM_out_tmp.nc

# -- Create two different files for land- and sea-mask
# --------------------------------------------------
# -- Comments: 
# -- setctomoss,0. = setting missing values to 0.
# -- gtc = greater than constant (o(t,x) = 1 if i(t,x) > const, o(t,x) = 0 else)
# -- ltc = lower than constant (o(t,x) = 1 if i(t,x) < const, o(t,x) = 0 else)
# --------------------------------------------------
cdo -L setctomiss,0. -ltc,0.5  LSM_in.nc oceanmask_in.nc
cdo -L setctomiss,0. -gec,0.5 LSM_in.nc landmask_in.nc
cdo -L setctomiss,0. -ltc,0.5 LSM_out_tmp.nc oceanmask_out.nc
cdo -L setctomiss,0. -gec,0.5 LSM_out_tmp.nc landmask_out.nc
cdo setrtoc2,0.5,1.0,1,0 LSM_out_tmp.nc LSM_out.nc
rm LSM_in.nc LSM_out_tmp.nc

# -- Select surface sea variables defined only on sea
ncks -h -v SST,CI data_in.nc datasea_in.nc

# -- Select surface variables defined on both that must be remapped differently on sea and on land
ncks -h -v SKT,STL1,STL2,STL3,STL4,ALB_SNOW,W_SNOW,T_SNOW data_in.nc dataland_in.nc

# -----------------------------------------------------------------------------
# -- Remap land and ocean area differently for variables
# -----------------------------------------------------------------------------

# -- Ocean part
# -----------------

# -- Apply the ocean mask (by dividing)
cdo div dataland_in.nc oceanmask_in.nc tmp1_land.nc
cdo div datasea_in.nc oceanmask_in.nc tmp1_sea.nc

# -- Set missing values to a distance-weighted average
cdo setmisstodis tmp1_land.nc tmp2_land.nc
cdo setmisstodis tmp1_sea.nc tmp2_sea.nc

# -- Remap 
cdo remap,triangular-grid.nc,weights.nc tmp2_land.nc tmp3_land.nc
cdo remap,triangular-grid.nc,weights.nc tmp2_sea.nc tmp3_sea.nc


# -- Apply the ocean mask to remapped variables (by dividing)
cdo div tmp3_land.nc oceanmask_out.nc dataland_ocean_out.nc
cdo div tmp3_sea.nc oceanmask_out.nc datasea_ocean_out.nc

# -- Clean the repository
rm tmp*.nc oceanmask*.nc

# -- Land part
# -----------------

cdo div dataland_in.nc landmask_in.nc tmp1.nc
cdo setmisstodis tmp1.nc tmp2.nc
cdo remap,triangular-grid.nc,weights.nc tmp2.nc tmp3.nc
cdo div tmp3.nc landmask_out.nc dataland_land_out.nc
rm tmp*.nc landmask*.nc dataland_in.nc datasea_in.nc

# -- merge remapped land and ocean part
# --------------------------------------

cdo ifthenelse LSM_out.nc dataland_land_out.nc dataland_ocean_out.nc dataland_out.nc
rm dataland_ocean_out.nc dataland_land_out.nc

# remap the rest and merge all files
# --------------------------------------

# -- Select all variables apart from these ones
ncks -h -x -v SKT,STL1,STL2,STL3,STL4,ALB_SNOW,W_SNOW,T_SNOW,SST,CI,LSM data_in.nc datarest_in.nc
# -- Remap
cdo -s remap,triangular-grid.nc,weights.nc datarest_in.nc era5_final.nc
rm datarest_in.nc data_in.nc

# -- Fill NaN values for SST and CI
cdo setmisstodis -selname,SST,CI datasea_ocean_out.nc dataland_ocean_out_filled.nc
rm datasea_ocean_out.nc

# -- Merge remapped files plus land-sea mask from EXTPAR
ncks -h -A dataland_out.nc era5_final.nc
ncks -h -A dataland_ocean_out_filled.nc era5_final.nc
ncks -h -A -v FR_LAND LSM_out.nc era5_final.nc
ncrename -h -v FR_LAND,LSM era5_final.nc
rm LSM_out.nc dataland_out.nc dataland_ocean_out_filled.nc

# ---------------------------------
# -- Post-processing
# ---------------------------------

ncrename -h -d cell,ncells era5_final.nc
ncrename -h -d nv,vertices era5_final.nc
ncks era5_final.nc {cfg.icon_input_icbc}/{cfg.meteo_prefix}{cfg.startdate_sim_yyyymmddhh}_ic{cfg.inidata_filename_suffix}
rm era5_final.nc

# -- Clean up
rm triangular-grid.nc weights.nc 
