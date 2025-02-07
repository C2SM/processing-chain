#!/bin/bash

cd {ERA5_folder}

{cfg.cdo_nco_cmd}

set -x

# 1. Remap
cdo griddes {filename} > triangular-grid.txt
cdo remapbil,triangular-grid.txt {CAMS_file} cams_triangle.nc

# 2. Write out the hybrid levels
cat >CAMS_levels.txt <<EOL
#
# zaxisID 1
#
zaxistype = hybrid
size      = 79
name      = level
longname  = "hybrid level at layer midpoints"
units     = "level"
levels    =  
EOL
ncks -v level cams_triangle.nc | sed -e '1,/data:/d' -e '$d' | sed 's/^[ ]*level = //' | sed 's/;$//'| tr -d '\n' >> CAMS_levels.txt
echo '' >> CAMS_levels.txt
echo 'vctsize   = 160' >> CAMS_levels.txt
echo 'vct       = ' >> CAMS_levels.txt
ncks -v ap cams_triangle.nc | sed -e '1,/data:/d' -e '$d' | sed 's/^[ ]*ap = //' | sed 's/;$//' | tr -d '\n' >> CAMS_levels.txt
ncks -v bp cams_triangle.nc | sed -e '1,/data:/d' -e '$d' | sed 's/^[ ]*bp = //' | sed 's/;$//' | tr -d '\n' >> CAMS_levels.txt
echo '' >> CAMS_levels.txt
echo 'formula = "hyam hybm (mlev=ap+bp*aps)"' >> CAMS_levels.txt
cdo setzaxis,CAMS_levels.txt cams_triangle.nc cams_withhybrid.nc

# 3. Add required variables
# --- CAMS
ncrename -O -v Psurf,PS -d level,lev -v level,lev cams_withhybrid.nc
ncap2 -s 'P0=1.0; lnsp=ln(PS); lev[lev]=array(0,1,$lev)' cams_withhybrid.nc -O cams_withhybrid_with_P.nc
ncks -C -v P0,PS,lnsp,CO2,hyam,hybm,hyai,hybi,lev,clon,clat cams_withhybrid_with_P.nc -O cams_light.nc
ncatted -a _FillValue,CO2,m,f,1.0e36 -O cams_light.nc
# --- ERA5
ncap2 -s 'P0=1.0; PS=PS(0,:)' {filename} -O data_in_with_P.nc
ncks -C -v hyam,hybm,hyai,hybi,clon,clat,P0 data_in_with_P.nc -O era5_light.nc
ncks -A -v PS cams_light.nc era5_light.nc

# 4. Remap
ncremap --no_stdin --vrt_fl=era5_light.nc -v CO2 cams_light.nc cams_remapped.nc
ncrename -O -d nhym,lev cams_remapped.nc

# 5. Place in inicond file
ncks -A -v CO2 cams_remapped.nc {filename}
ncap2 -s 'M_Air=28.9647; M_CO2=44.01; CO2_new[time,lev,ncells]=CO2*(M_CO2/M_Air)*(1-QV);' {filename}
ncks -C -O -x -v CO2 {filename} tmp.nc
ncrename -v CO2_new,CO2 tmp.nc

# 6. Remap to lateral boundaries
cat > NAMELIST_ICONSUB << EOF_1
&iconsub_nml
  grid_filename    = '{cfg.input_files_scratch_dynamics_grid_filename}',
  output_type      = 4,
  lwrite_grid      = .TRUE.,
/
&subarea_nml
  ORDER            = "lateral_boundary",
  grf_info_file    = '{cfg.input_files_scratch_dynamics_grid_filename}',
  min_refin_c_ctrl = 1
  max_refin_c_ctrl = 120
/
EOF_1

iconsub --nml NAMELIST_ICONSUB

cdo selgrid,2 lateral_boundary.grid.nc triangular-grid_00_lbc.nc
cdo remapdis,triangular-grid_00_lbc.nc tmp.nc {era5_cams_nudge_file}
ncrename -d cell,ncells {era5_cams_nudge_file}
ncrename -d nv,vertices {era5_cams_nudge_file}
{cfg.cdo_nco_cmd_post}
