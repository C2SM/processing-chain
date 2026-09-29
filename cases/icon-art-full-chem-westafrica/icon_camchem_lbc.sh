#!/bin/bash

cd {cfg.icon_input_icbc}

# -- Loop over file list
i=0
echo "DATAFILELIST is {datafile_list_chem}"
for datafilename in {datafile_list_chem} ; do
	datafile="${{datafilename##*/}}"  # get filename without path
	outdatafile=${{datafile%.*}}      # get filename without suffix
    ((i++))

    # ---------------------------------
    # -- Pre-processing
    # ---------------------------------

    rm -f {cfg.icon_input_icbc}/${{outdatafile}}_icon{cfg.inidata_filename_suffix}

    # -- Change variable and coordinates names to be consistent with ICON nomenclature
    cdo setpartabn,partab_chem,convert $datafilename data_in.nc

    # ---------------------------------
    # -- Re-mapping
    # ---------------------------------

    # -- Retrieve the lateral boundary grid 
    cdo -s selgrid,2 {cfg.input_files_scratch_lateral_boundary_grid} triangular-grid.nc

    # -- Create the weights for remapping CAM-Chem data from latlon grid onto the triangular grid
    if [[ $i == "1" ]] ; then
        echo "creating weights"
        cdo genbil,triangular-grid.nc data_in.nc weights.nc
    fi

    # -- Remap
    cdo -s remap,triangular-grid.nc,weights.nc data_in.nc chem_final.nc
    rm data_in.nc triangular-grid.nc

    # ---------------------------------
    # -- Post-processing
    # ---------------------------------

    # -- Rename dimensions and order alphabetically
    ncrename -h -d cell,ncells chem_final.nc
    ncrename -h -d nv,vertices chem_final.nc
    ncks chem_final.nc {cfg.icon_input_icbc}/${{outdatafile}}_icon{cfg.inidata_filename_suffix}
    rm chem_final.nc 

    # -- Clean up
    rm $datafilename
done
# -- Clean up
rm weights.nc
