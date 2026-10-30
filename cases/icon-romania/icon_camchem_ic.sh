#!/bin/bash

set -euo pipefail
# export PATH=$HOME/envs/nctools/bin:$PATH
export PATH=/capstor/scratch/cscs/msteiner/envs/nctools/bin:$PATH

cd {cfg.icon_input_icbc}

# ---------------------------------
# -- Pre-processing
# ---------------------------------

# rm -f {cfg.icon_input_icbc}/{cfg.chem_prefix}{cfg.startdate_sim_yyyymmddhh}_ic_icon{cfg.inidata_filename_suffix}

# -- Change variable and coordinates names to be consistent with MECCA nomenclature
# cdo setpartabn,partab_chem,convert {cfg.icon_input_icbc}/{cfg.chem_prefix}{cfg.startdate_sim_yyyymmddhh}_ic{cfg.chem_suffix} $out

# f={cfg.icon_input_icbc}/{cfg.chem_prefix}{cfg.startdate_sim_yyyymm}{cfg.chem_suffix}
# d={cfg.startdate_sim_yyyymmddhh}
# out={cfg.icon_input_icbc}/{cfg.chem_prefix}${{d}}_ic{cfg.chem_suffix}

# -- Input (monthly lat/lon file) and final file (monthly chemistry on the
# -- ICON grid, input for vert_intpl) are set by prepare_art.py, once per
# -- chemistry source (CAMS CH4, EAC4 C2H6, ...)
in={chem_in}
final={chem_out}
out=${{final%.nc}}_latlon_tmp.nc
rm -f $final

cp $in $out

# # 2023030100 -> 2023-03-01T00:00:00
# cdo -s seldate,${{d:0:4}}-${{d:4:2}}-${{d:6:2}}T${{d:8:2}}:00:00 $f $out

# ---------------------------------
# -- Re-mapping
# ---------------------------------

# -- Retrieve the triangular horizontal grid
cdo -s selgrid,1 {cfg.input_files_scratch_dynamics_grid_filename} triangular-grid.nc

# -- Create the weights for remapping CAM-Chem (lat,lon) grid onto the triangular grid
echo "creating weights"
cdo genbil,triangular-grid.nc $out weights.nc

# -- Remap
cdo -s remap,triangular-grid.nc,weights.nc $out chem_final.nc

# -- Restore hybrid coefficients (CDO drops them when remapping; dims level/hlevel are kept)
ncks -A -C -v hyam,hybm,hyai,hybi $out chem_final.nc
rm $out triangular-grid.nc

# ncks -A -C -v CH4, Q chem_final.nc {cfg.icon_input_icbc}/{cfg.meteo_prefix}{cfg.startdate_sim_yyyymmddhh}{cfg.inidata_filename_suffix}

# ---------------------------------
# -- Post-processing
# ---------------------------------

# -- Rename dimensions and order alphabetically
ncrename -h -d cell,ncells chem_final.nc
# ncrename -h -d nv,vertices chem_final.nc
ncks chem_final.nc $final
rm chem_final.nc weights.nc 

# -- Clean up
# rm {cfg.icon_input_icbc}/{cfg.chem_prefix}{cfg.startdate_sim_yyyymmddhh}_ic{cfg.chem_suffix}
