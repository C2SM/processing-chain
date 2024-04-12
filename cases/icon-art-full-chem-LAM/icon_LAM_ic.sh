#!/bin/bash

cd {cfg.icon_input_icbc}

# ---------------------------------
# -- Pre-processing
# ---------------------------------

rm -f {cfg.icon_input_icbc}/{cfg.inidata_prefix}{cfg.startdate_sim_yyyymmddhh}_tmp{cfg.inidata_filename_suffix}

# -- Change variable and coordinates names to be consistent with ICON nomenclature
cdo setpartabn,partab_LAM,convert {cfg.LAM_icon_output_dir}/{cfg.LAM_prefix}IC_{date_icon_out_format}{cfg.LAM_suffix} data_in.nc

# ---------------------------------
# -- Re-mapping
# ---------------------------------

# -- Retrieve the triangular horizontal grid
cdo -s selgrid,2 {cfg.input_files_scratch_dynamics_grid_filename} triangular-grid.nc

# -- Create the weights for remapping 
echo "creating weights"
cdo gendis,triangular-grid.nc data_in.nc weights.nc

# -- Remap
cdo -s remap,triangular-grid.nc,weights.nc data_in.nc lam_final.nc
rm data_in.nc triangular-grid.nc

# ---------------------------------
# -- Post-processing
# ---------------------------------

ncks lam_final.nc {cfg.icon_input_icbc}/{cfg.inidata_prefix}{cfg.startdate_sim_yyyymmddhh}_tmp{cfg.inidata_filename_suffix} 
rm lam_final.nc weights.nc 