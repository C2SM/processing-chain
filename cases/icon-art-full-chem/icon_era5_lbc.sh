#!/bin/bash

cd {cfg.icon_input_icbc}

# -- Loop over file list
i=0
for datafilename in {datafile_list_meteo} ; do
    datafile="${{datafilename##*/}}"  # get filename without path
	outdatafile=${{datafile%.*}}      # get filename without suffix
    ((i++))

    # ---------------------------------
    # -- Pre-processing
    # ---------------------------------

    rm -f {cfg.icon_input_icbc}/${{outdatafile}}_lbc{cfg.inidata_filename_suffix}

    # -- Convert the GRIB files to NetCDF
    cdo -t ecmwf -f nc copy $datafilename era5_ori.nc    

    # -- Change variable and coordinates names to be consistent with ICON nomenclature
    cdo setpartabn,partab_meteo,convert era5_ori.nc tmp.nc
    # -- Order the variables alphabetically 
    ncks tmp.nc data_in.nc
    rm tmp.nc era5_ori.nc

    # ---------------------------------
    # -- Re-mapping
    # ---------------------------------

    # -- Retrieve the lateral boundary grid 
    cdo -s selgrid,2 {cfg.input_files_scratch_lateral_boundary_grid} triangular-grid.nc

    # -- Create the weights for remapping era5 latlon grid onto the triangular grid
    if [[ $i == "1" ]] ; then
        echo "creating weights"
        cdo gendis,triangular-grid.nc data_in.nc weights.nc
    fi

    # -- Remap
    cdo -s remap,triangular-grid.nc,weights.nc data_in.nc era5_final.nc
    rm data_in.nc triangular-grid.nc

    # ---------------------------------
    # -- Post-processing
    # ---------------------------------

    ncrename -h -d cell,ncells era5_final.nc
    ncrename -h -d nv,vertices era5_final.nc
    ncks era5_final.nc {cfg.icon_input_icbc}/${{outdatafile}}_lbc{cfg.inidata_filename_suffix}
    rm era5_final.nc
done
# -- Clean up
rm weights.nc