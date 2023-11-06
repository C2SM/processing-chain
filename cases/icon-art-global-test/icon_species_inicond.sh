#!/bin/bash

cd {cfg.icon_input_icbc}


species2restart=($(echo {cfg.species2restart} | tr -d '[],'))


if [[ {cfg.lrestart} == '.FALSE.' ]]; then

    # ----------------------------------------------------------
    # -- Replicate Q and GEOSP variables for ICON-ART
    # ----------------------------------------------------------

    cdo expr,'Q=QV;' {filename} tmp.nc
    ncks -A -v Q tmp.nc {filename}
    rm tmp.nc

    cdo expr,'GEOP_SFC=GEOSP;' {filename} tmp.nc
    ncks -A -v GEOP_SFC tmp.nc {filename}
    rm tmp.nc

fi

# ----------------------------------------------------------
# -- Create CH4 and CO variables (if CAMS not available)
# ----------------------------------------------------------
if [[ "${{species2restart[*]}}" =~ "TRCH4" || "${{species2restart[*]}}" =~ "TRCO" ]]; then

    # ----------------------------------------------------------
    # -- Remap CAMS data (if CAMS available...)
    # ----------------------------------------------------------

    # # -- Convert the GRIB files to NetCDF
    # cdo -t ecmwf -f nc copy cams.grib cams.nc

    # # -- Retrieve the dynamic horizontal grid
    # cdo -s selgrid,2 {cfg.input_files_scratch_dynamics_grid_filename} triangular-grid.nc

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
    # -- Or just create basic variables
    # ----------------------------------------------------------
    var_tracer="TRCH4{ext_restart}"
    cdo expr,"TRCH4=QV / QV * 0.000002;" {filename} tmp.nc
    if [ ! -z "{ext_restart}" ] ; then
        ncrename -h -v TRCH4,${{var_tracer}} tmp.nc 
    fi 
    ncks -A -v ${{var_tracer}} tmp.nc {filename}
    rm tmp.nc

    var_tracer="TRCO{ext_restart}"
    cdo expr,"TRCO=QV / QV * 0.000002;" {filename} tmp.nc
    if [ ! -z "{ext_restart}" ] ; then
        ncrename -h -v TRCO,${{var_tracer}} tmp.nc 
    fi 
    ncks -A -v ${{var_tracer}} tmp.nc {filename}
    rm tmp.nc


fi

