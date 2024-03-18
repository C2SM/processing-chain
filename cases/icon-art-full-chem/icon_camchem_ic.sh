#!/bin/bash

cd {cfg.icon_input_icbc}

# ---------------------------------
# -- Pre-processing
# ---------------------------------

rm -f {cfg.icon_input_icbc}/{cfg.chem_prefix}{cfg.startdate_sim_yyyymmddhh}_ic_icon{cfg.inidata_filename_suffix}

# -- Change variable and coordinates names to be consistent with MECCA nomenclature
cdo setpartabn,partab_chem,convert {cfg.icon_input_icbc}/{cfg.chem_prefix}{cfg.startdate_sim_yyyymmddhh}_ic{cfg.chem_suffix} data_in.nc

# ---------------------------------
# -- Re-mapping
# ---------------------------------

# -- Retrieve the triangular horizontal grid
cdo -s selgrid,2 {cfg.input_files_scratch_dynamics_grid_filename} triangular-grid.nc

# -- Create the weights for remapping CAM-Chem (lat,lon) grid onto the triangular grid
echo "creating weights"
cdo genbil,triangular-grid.nc data_in.nc weights.nc

# -- Remap
cdo -s remap,triangular-grid.nc,weights.nc data_in.nc chem_final.nc
rm data_in.nc triangular-grid.nc

# ---------------------------------
# -- Post-processing
# ---------------------------------

# -- Rename dimensions and order alphabetically
ncrename -h -d cell,ncells chem_final.nc
ncrename -h -d nv,vertices chem_final.nc
ncks chem_final.nc {cfg.icon_input_icbc}/{cfg.chem_prefix}{cfg.startdate_sim_yyyymmddhh}_ic_icon{cfg.inidata_filename_suffix} 
rm chem_final.nc weights.nc 

# -- Clean up
rm {cfg.icon_input_icbc}/{cfg.chem_prefix}{cfg.startdate_sim_yyyymmddhh}_ic{cfg.chem_suffix}
