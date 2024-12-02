#!/bin/bash

cd {ERA5_folder}

module load daint-mc CDO
source ~/miniconda3/bin/activate
conda init bash
source ~/.bashrc
conda activate /scratch/snx3000/ekoene/conda/NCO

# 1. Remap
cdo griddes {filename} > triangular-grid.txt
cdo remapnn,triangular-grid.txt {CAMS_file} cams_triangle.nc

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
ncremap --vrt_fl=era5_light.nc -v CO2 cams_light.nc cams_remapped.nc
ncrename -O -d nhym,lev cams_remapped.nc

# 5. Place in inicond file
ncks -A -v CO2 cams_remapped.nc {filename}
ncap2 -s 'CO2_new[time,lev,ncells]=CO2; CO2=CO2_new;' {filename}
ncks -C -O -x -v CO2_new {filename} {cfg.icon_input_icbc}/$(basename {filename})
