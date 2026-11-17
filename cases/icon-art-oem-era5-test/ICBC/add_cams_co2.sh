#!/bin/bash
# ==========================================================================
# Add CAMS CO2 into each ERA5 initial/boundary-condition file written by
# remap_era5.sh.
#
# Input:  {outdir}/era5_ini_<YYYYMMDDHH>.nc     (from remap_era5.sh)
#         {cams_dir}/cams_co2_<YYYYMMDDTHH>.nc  (from fetch_cams_co2_months())
# Output: {outdir}/era5_ini_<YYYYMMDDHH>.nc, CO2 added in place
#         (mass mixing ratio, converted from CAMS's mol/mol)
#
# Safe to re-run: a file that already has a CO2 variable is skipped, so an
# interrupted or repeated run won't double-convert it.
#
# Locked (flock) so that overlapping invocations serialize instead of both
# writing into the shared WORKDIR at once.
# ==========================================================================
set -euo pipefail

# Exported: the cdo/nco setup below wraps the rest of this script inside a
# `uenv start ... << EOF` heredoc, which runs as a separate process that
# only inherits exported variables from this shell.
export ERA5_ICON_DIR="{outdir}"
export CAMS_DIR="{cams_dir}"
export WORKDIR="{workdir}"

exec 200>"$WORKDIR/add_cams_co2.lock"
flock 200

{cfg.cdo_nco_cmd}

mkdir -p "$WORKDIR"
cd "$WORKDIR"

# Only this chunk's timesteps: the directory may also hold a link to the
# experiment start file of the first chunk
for timestamp in {era5_timestamps}; do

    inicond_filename="$ERA5_ICON_DIR/era5_ini_${{timestamp}}.nc"
    if [ ! -f "$inicond_filename" ]; then
        echo "ERROR: $inicond_filename not found (did remap_era5.sh run?)" >&2
        exit 1
    fi

    cams_timestamp="${{timestamp:0:8}}T${{timestamp:8:2}}"
    CAMS_file="$CAMS_DIR/cams_co2_${{cams_timestamp}}.nc"

    if [ ! -f "$CAMS_file" ]; then
        echo "No matching CAMS file for $timestamp (looked for $CAMS_file), skipping." >&2
        continue
    fi

    if ncks -m -v CO2 "$inicond_filename" >/dev/null 2>&1; then
        echo "$inicond_filename already has CO2, skipping."
        continue
    fi

    echo "Processing $timestamp"

    # 1. Remap CAMS onto the ERA5 file's own (already-triangular) grid
    cdo griddes "$inicond_filename" > triangular-grid.txt
    cdo remapbil,triangular-grid.txt "$CAMS_file" cams_triangle.nc

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
    # -- ncremap takes the source surface pressure from lnsp, so PS must not
    # -- also be in the source file
    ncks -C -v P0,lnsp,CO2,hyam,hybm,hyai,hybi,lev,clon,clat cams_withhybrid_with_P.nc -O cams_light.nc
    ncatted -a _FillValue,CO2,m,f,1.0e36 -O cams_light.nc
    # --- ERA5 (work on a copy so the real file is only touched by the
    # -- final mv, in case a later step fails partway through)
    cp "$inicond_filename" inicond_work.nc
    ncap2 -s 'P0=1.0; PS=PS(0,:)' inicond_work.nc -O data_in_with_P.nc
    ncks -C -v hyam,hybm,hyai,hybi,clon,clat,P0 data_in_with_P.nc -O era5_light.nc
    ncks -A -v PS cams_withhybrid_with_P.nc era5_light.nc

    # 4. Remap CO2 vertically onto the ERA5 file's hybrid levels (needs
    # -- NCO 4.9.0 or >= 5.3.2; e.g. 5.1.5 to 5.3.1 skip IFS-format hybrid
    # -- data)
    ncremap --no_stdin --ps_nm=lnsp --vrt_fl=era5_light.nc -v CO2 cams_light.nc cams_remapped.nc
    ncrename -O -d nhym,lev cams_remapped.nc

    # 5. Place in inicond file, converting CO2 from CAMS's mol/mol to
    # -- ICON's mass mixing ratio, and clean up naming/dimensions
    ncks -A -v CO2 cams_remapped.nc inicond_work.nc
    ncap2 -O -s 'M_Air=28.9647; M_CO2=44.01; CO2_new[time,lev,ncells]=CO2*(M_CO2/M_Air);' inicond_work.nc inicond_work.nc
    ncks -C -O -x -v CO2,M_Air,M_CO2 inicond_work.nc inicond_final.nc # Keep only the converted CO2
    ncrename -v CO2_new,CO2 inicond_final.nc # Rename CO2_new to CO2
    ncrename -d .cell,ncells inicond_final.nc
    ncrename -d .nv,vertices inicond_final.nc

    mv inicond_final.nc "$inicond_filename"

    rm -f triangular-grid.txt CAMS_levels.txt cams_triangle.nc cams_withhybrid.nc \
          cams_withhybrid_with_P.nc cams_light.nc inicond_work.nc data_in_with_P.nc \
          era5_light.nc cams_remapped.nc

    echo "Updated $inicond_filename with CO2 from $CAMS_file"

done

{cfg.cdo_nco_cmd_post}
