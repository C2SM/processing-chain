#!/bin/bash

cd {cfg.icon_input_icbc}

# -- Loop over file list
i=0
echo "DATAFILELIST is {datafile_list}"
for datafilename in {datafile_list} ; do
	datafile="${{datafilename##*/}}"  # get filename without path
	outdatafile=${{datafile%.*}}      # get filename without suffix
    ((i++))

    # ---------------------------------
    # -- Pre-processing
    # ---------------------------------

    rm -f {cfg.icon_input_icbc}/${{outdatafile}}_tmp{cfg.lbcdata_filename_suffix}

    # -- Change variable and coordinates names to be consistent with ICON nomenclature
    cdo setpartabn,partab_LAM,convert $datafilename data_in.nc

    # ---------------------------------
    # -- Re-mapping
    # ---------------------------------

    # -- Retrieve the lateral boundary grid 
    cdo -s selgrid,2 {cfg.input_files_scratch_lateral_boundary_grid} triangular-grid.nc

    # -- Create the weights for remapping CAM-Chem data from latlon grid onto the triangular grid
    if [[ $i == "1" ]] ; then
        echo "creating weights"
        cdo gendis,triangular-grid.nc data_in.nc weights.nc
    fi

    # -- Remap
    cdo -s remap,triangular-grid.nc,weights.nc data_in.nc chem_final.nc
    rm data_in.nc triangular-grid.nc

    # ---------------------------------
    # -- Post-processing
    # ---------------------------------

    ncks chem_final.nc {cfg.icon_input_icbc}/${{outdatafile}}_tmp{cfg.lbcdata_filename_suffix}
    rm chem_final.nc 
    
done
# -- Clean up
rm weights.nc