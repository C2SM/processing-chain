#!/bin/bash

set -euo pipefail
# export PATH=$HOME/envs/nctools/bin:$PATH
export PATH=/capstor/scratch/cscs/msteiner/envs/nctools/bin:$PATH

cd {cfg.icon_input_icbc}

# ---------------------------------
# -- Pre-processing
# ---------------------------------

# -- Change variable and coordinates names to be consistent with ICON nomenclature
# cdo setpartabn,partab_chem,convert $in data_in.nc

# -- Input (monthly lat/lon file) and final file (monthly chemistry on the
# -- lateral boundary grid, input for vert_intpl) are set by prepare_art.py,
# -- once per chemistry source (CAMS CH4, EAC4 C2H6, ...)
in={chem_in}
final={chem_out}
rm -f $final

# ---------------------------------
# -- Re-mapping
# ---------------------------------

# -- Retrieve the lateral boundary grid
cdo -s selgrid,2 {cfg.input_files_scratch_lateral_boundary_grid} lbc-grid.nc

# -- Create the weights for remapping CAM-Chem (lat,lon) grid onto the lateral boundary grid
echo "creating weights"
cdo genbil,lbc-grid.nc $in weights_lbc.nc

# -- Remap
cdo -s remap,lbc-grid.nc,weights_lbc.nc $in chem_final_lbc.nc

# -- Restore hybrid coefficients (CDO drops them when remapping; dims level/hlevel are kept)
ncks -A -C -v hyam,hybm,hyai,hybi $in chem_final_lbc.nc
rm lbc-grid.nc

# ---------------------------------
# -- Post-processing
# ---------------------------------

# -- Rename dimensions (the '.' makes the rename optional)
ncrename -h -d cell,ncells chem_final_lbc.nc
ncrename -h -d .nv,vertices chem_final_lbc.nc
ncks chem_final_lbc.nc $final
rm chem_final_lbc.nc weights_lbc.nc
