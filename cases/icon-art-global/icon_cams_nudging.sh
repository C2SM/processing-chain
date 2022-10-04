#!/bin/bash

cd {cfg.icon_input_icbc}

# ----------------------------------------------------------
# -- Replicate Q and GEOSP variables for ICON-ART
# ----------------------------------------------------------

cdo expr,'Q=QV;' {filename} tmp.nc
ncks -A -v Q tmp.nc {filename}
rm tmp.nc

cdo expr,'GEOP_SFC=GEOSP;' {filename} tmp.nc
ncks -A -v GEOP_SFC tmp.nc {filename}
rm tmp.nc

# ----------------------------------------------------------
# -- Remap CAMS data (if CAMS available)
# ----------------------------------------------------------

# # -- Convert the GRIB files to NetCDF
# cdo -t ecmwf -f nc copy cams.grib cams.nc

# # -- Retrieve the dynamic horizontal grid
# cdo -s selgrid,2 {cfg.dynamics_grid_filename_scratch} triangular-grid.nc

# # -- Remap
# cdo -s remapdis,triangular-grid.nc cams.nc cams_final.nc
# rm cams.nc 

# # -- Merge CAMS and ERA5 data
# ncks -h -A cams.nc tmp.nc
# rm cams.nc cams.grib

# # -- Rename variables
# ncrename -h CH4,TRCH4 tmp.nc
# ncrename -h CO,TRCO tmp.nc
# ncks tmp.nc {filename} 
# rm tmp.nc

# ----------------------------------------------------------
# -- Create CH4 and CO variables (if CAMS not available)
# ----------------------------------------------------------

cdo expr,'TRCH4=QV / QV * 0.000002;' {filename} tmp.nc
ncks -A -v TRCH4 tmp.nc {filename}
rm tmp.nc

cdo expr,'TRCO=QV / QV * 0.0000002;' {filename} tmp.nc
ncks -A -v TRCO tmp.nc {filename}
rm tmp.nc
