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

# -- Change variable and coordinates names to be consistent with ICON nomenclature
cdo setpartabn,mypartab,convert era5_original.nc tmp.nc

# -- Order the variables alphabetically 
ncks tmp.nc data_in.nc
rm tmp.nc era5_surf.nc era5_ml.nc

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

# -- Select surface sea variables defined only on sea
ncks -h -v SST,CI data_in.nc datasea_in.nc

# -- Select surface variables defined on both that must be remap differently on sea and on land
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
cdo remapdis,triangular-grid.nc tmp2_land.nc tmp3_land.nc
cdo remapdis,triangular-grid.nc tmp2_sea.nc tmp3_sea.nc

# -- Apply the ocean mask to remapped variables (by dividing)
cdo div tmp3_land.nc oceanmask_out.nc dataland_ocean_out.nc
cdo div tmp3_sea.nc oceanmask_out.nc datasea_ocean_out.nc

# -- Clean the repository
rm tmp*.nc oceanmask*.nc

# # -- Land part
# # -----------------

cdo div dataland_in.nc landmask_in.nc tmp1.nc
cdo setmisstodis tmp1.nc tmp2.nc
cdo remapdis,triangular-grid.nc tmp2.nc tmp3.nc
cdo div tmp3.nc landmask_out.nc dataland_land_out.nc
rm tmp*.nc landmask*.nc dataland_in.nc

# -- merge remapped land and ocean part
# --------------------------------------

cdo ifthenelse LSM_out.nc dataland_land_out.nc dataland_ocean_out.nc dataland_out.nc
rm dataland_ocean_out.nc dataland_land_out.nc

# remap the rest and merge all files
# --------------------------------------

# -- Select all variables apart from these ones
ncks -h -x -v SKT,STL1,STL2,STL3,STL4,SMIL1,SMIL2,SMIL3,SMIL4,ALB_SNOW,W_SNOW,T_SNOW,SST,CI,LSM data_in.nc datarest_in.nc

# -- Remap
cdo -s remapdis,triangular-grid.nc datarest_in.nc era5_final.nc
rm datarest_in.nc

# -- Fill NaN values for SST and CI
cdo setmisstodis -selname,SST,CI datasea_ocean_out.nc dataland_ocean_out_filled.nc

# -- Merge remapped files plus land sea mask from EXTPAR
ncks -h -A dataland_out.nc era5_final.nc
ncks -h -A dataland_ocean_out_filled.nc era5_final.nc
ncks -h -A -v FR_LAND LSM_out.nc era5_final.nc
ncrename -h -v FR_LAND,LSM era5_final.nc
rm LSM_out.nc dataland_out.nc

# ------------------------------------------------------------------------
# -- Convert the (former) SWVLi variables to real soil moisture indices
# ------------------------------------------------------------------------

# -- Properties of IFS soil types (see Table 1 ERA5 Data documentation 
# -- https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation)
# Soil type   1     2     3     4     5     6     7
wiltingp=(0 0.059 0.151 0.133 0.279 0.335 0.267 0.151) # wilting point
fieldcap=(0 0.244 0.347 0.383 0.448 0.541 0.663 0.347) # field capacity

ncks -h -v SMIL1,SMIL2,SMIL3,SMIL4,SLT data_in.nc swvl.nc
rm data_in.nc

# -- Loop over the soil types and apply the right constants
smi_equation=""
for ilev in {{1..4}}; do

    smi_equation="${{smi_equation}}SMIL${{ilev}} = (SMIL${{ilev}} - ${{wiltingp[1]}}) / (${{fieldcap[1]}} - ${{wiltingp[1]}}) * (SLT==1)"
    for ist in {{2..7}}; do
        smi_equation="${{smi_equation}} + (SMIL${{ilev}} - ${{wiltingp[$ist]}}) / (${{fieldcap[$ist]}} - ${{wiltingp[$ist]}}) * (SLT==${{ist}})"
    done
    smi_equation="${{smi_equation}};"

done

cdo expr,"${{smi_equation}}" swvl.nc smil_in.nc
rm swvl.nc

# -- Remap SMIL variables
cdo -s remapdis,triangular-grid.nc smil_in.nc smil_out.nc
rm smil_in.nc

# -- Overwrite the variables SMIL1,SMIL2,SMIL3,SMIL4
ncks -A -v SMIL1,SMIL2,SMIL3,SMIL4 smil_out.nc era5_final.nc
rm smil_out.nc

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
ncks era5_final.nc {cfg.inicond_filename_scratch} 
rm era5_final.nc

# -- Clean the repository
rm weights.nc
rm triangular-grid.nc