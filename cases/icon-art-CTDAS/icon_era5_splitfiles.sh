#!/bin/bash

cd {ERA5_folder}

module load daint-mc CDO NCO

# Loop over ml and surf files
for ml_file in {ml_files}; do
    # Convert GRIB to NetCDF for ml file and then process it
    cdo -t ecmwf -f nc copy "${{ml_file}}" "${{ml_file%.grib}}.nc"

    # Show timestamp and split for ml file
    cdo showtimestamp "${{ml_file%.grib}}.nc" > list_ml.txt
    cdo -splitsel,1 "${{ml_file%.grib}}.nc" split_ml_

    times_ml=($(cat list_ml.txt))
    x=0
    for f in $(ls split_ml_*.nc); do
        mv $f era5_ml_${{times_ml[$x]}}.nc
        let x=$x+1
    done
done

for surf_file in {surf_files}; do
    # Convert GRIB to NetCDF for surf file and then process it
    cdo -t ecmwf -f nc copy "${{surf_file}}" "${{surf_file%.grib}}.nc"

    # Show timestamp and split for surf file
    cdo showtimestamp "${{surf_file%.grib}}.nc" > list_surf.txt
    cdo -splitsel,1 "${{surf_file%.grib}}.nc" split_surf_

    times_surf=($(cat list_surf.txt))
    y=0
    for f in $(ls split_surf_*.nc); do
        mv $f era5_surf_${{times_surf[$y]}}.nc
        let y=$y+1
    done
done
