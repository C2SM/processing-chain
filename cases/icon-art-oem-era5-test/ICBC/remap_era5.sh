#!/bin/bash
# ==========================================================================
# Remap ERA5 files fetched by fetch_era5_arco() onto the ICON triangular
# grid, and assemble the final ICON initial/boundary-condition files.
#
# fetch_era5_arco() already handles everything that doesn't need remapping:
#   - fetching + merging ml/surf fields from the ARCO-ERA5 zarr stores
#   - renaming variables/coords to ICON nomenclature
#   - hybrid-level coefficients (hyam/hybm/hyai/hybi)
#   - soil depth coordinates
#   - SWVLi -> SMIL soil-moisture-index conversion (on the source grid,
#     since it depends on the per-pixel source-grid soil type)
#   - Q/GEOSP aliases for QV/GEOP_SFC (plain duplicates, no remap-order
#     constraint, so no reason not to create them pre-remap)
#
# This script picks up from there: land/sea-aware remapping (SMIL1-4 are
# treated as land-only, same as STL1-4, since soil moisture like soil
# temperature is undefined over ocean), LNPS creation (done post-remap so
# it stays consistent with the remapped PS in the same file), and final
# renaming.
#
# Input:  {era5_dir}/ERA5_*.nc
# Output: {outdir}/era5_ini_<timestamp>.nc
# ==========================================================================
set -euo pipefail

# Exported: the cdo/nco setup below wraps the rest of this script inside a
# `uenv start ... << EOF` heredoc, which runs as a separate process that
# only inherits exported variables from this shell.
export ERA5_DIR="{era5_dir}"
export OUTDIR="{outdir}"
export WORKDIR="{workdir}"
export DYNAMICS_GRID_FILE="{cfg.input_files_dynamics_grid_filename}"
export EXTPAR_FILE="{cfg.input_files_extpar_filename}"
export INICOND_PREFIX="era5_ini"
export ERA5_TIMESTAMPS="{era5_timestamps}"

{cfg.cdo_nco_cmd}

mkdir -p "$OUTDIR" "$WORKDIR"
cd "$WORKDIR"

# Only this chunk's own ERA5 timesteps, not every ERA5_*.nc file that
# happens to sit in the shared global_inputs/ERA5 directory (that
# directory accumulates data fetched by every chunk over the whole
# simulation, not just this one).
input_files=()
for ts in $ERA5_TIMESTAMPS; do
    f="$ERA5_DIR/ERA5_${{ts}}.nc"
    if [ ! -f "$f" ]; then
        echo "ERROR: expected ERA5 file not found: $f (did get_icbc_data run for this chunk?)" >&2
        exit 1
    fi
    input_files+=("$f")
done
if [ ${{#input_files[@]}} -eq 0 ]; then
    echo "No ERA5 timesteps needed for this chunk" >&2
    exit 1
fi

# ------------------------------------------------------------------------
# -- One-time setup: target grid + remap weights.
# -- The source (ERA5 lat-lon slice) and target (ICON triangular) grids
# -- are identical for every timestep, so both are computed once here and
# -- reused via `cdo remap,triangular-grid.nc,weights.nc` in the loop below.
#
# -- ICON grid files often bundle several CDI grid definitions (cell,
# -- vertex, and edge topologies, sometimes more than once each -- e.g.
# -- once per variable that happens to use it) -- which index is "the"
# -- cell grid isn't standardized across grid-generation tools, so a
# -- hardcoded `selgrid,N` isn't reliable across different ICON grid
# -- files. Instead, find the grid with nvertex=3: a triangular ICON cell
# -- grid has exactly 3 vertices per cell by definition, and (unlike e.g.
# -- matching on gridsize) that's true of exactly one grid definition in
# -- the file, with no ambiguity from duplicate registrations.
# ------------------------------------------------------------------------
cell_grid=$(cdo -s griddes "$DYNAMICS_GRID_FILE" | awk '
    /# gridID/ {{ gid = $3 }}
    /nvertex[ ]*=/ {{
        if (result == "" && $NF == "3") {{
            result = gid
        }}
    }}
    END {{ print result }}
')
if [ -z "$cell_grid" ]; then
    echo "ERROR: no CDI grid definition in $DYNAMICS_GRID_FILE has nvertex=3 (triangular cells)" >&2
    exit 1
fi
cdo -s selgrid,"$cell_grid" "$DYNAMICS_GRID_FILE" triangular-grid.nc
cdo gendis,triangular-grid.nc "${{input_files[0]}}" weights.nc

# -- Cell vertex bounds of the ICON grid, with the dimension names of the
# -- output files; added to the output if the remapping does not write
# -- them (it depends on the CDO version)
ncks -O -C -v clon_bnds,clat_bnds triangular-grid.nc grid_bnds.nc
ncrename -h -d cell,ncells -d nv,vertices grid_bnds.nc

# -- ERA5's LSM and the EXTPAR FR_LAND are both time-invariant, so the
# -- land/ocean masks derived from them are computed once here (from
# -- input_files[0]) and reused for every timestep below.
cdo selname,LSM "${{input_files[0]}}" LSM_in.nc
ncrename -h -v LSM,FR_LAND LSM_in.nc
cdo selname,FR_LAND "$EXTPAR_FILE" LSM_out_tmp.nc

# -- Add time dimension to LSM_out.nc
ncecat -O -u time LSM_out_tmp.nc LSM_out_tmp.nc
ncks -h -A -v time LSM_in.nc LSM_out_tmp.nc

# -- Create two different files for land- and sea-mask
cdo -L setctomiss,0. -ltc,0.5 LSM_in.nc oceanmask_in.nc
cdo -L setctomiss,0. -gec,0.5 LSM_in.nc landmask_in.nc
cdo -L setctomiss,0. -ltc,0.5 LSM_out_tmp.nc oceanmask_out.nc
cdo -L setctomiss,0. -gec,0.5 LSM_out_tmp.nc landmask_out.nc
cdo setrtoc2,0.5,1.0,1,0 LSM_out_tmp.nc LSM_out.nc
rm LSM_in.nc LSM_out_tmp.nc

for data_in_src in "${{input_files[@]}}"; do

    timestamp=$(basename "$data_in_src" .nc | sed 's/^ERA5_//')

    inicond_filename="$OUTDIR/${{INICOND_PREFIX}}_${{timestamp}}.nc"
    if [ -f "$inicond_filename" ]; then
        echo "Already remapped, skipping: $inicond_filename"
        continue
    fi

    echo "Processing $timestamp"

    cp "$data_in_src" data_in.nc

    # ---------------------------------
    # -- Re-mapping
    # ---------------------------------

    # -- Select surface sea variables defined only on sea
    ncks -O -h -v SST,CI data_in.nc datasea_in.nc

    # -- Select surface variables defined on both that must be remapped
    # -- differently on sea and on land
    ncks -O -h -v SKT,STL1,STL2,STL3,STL4,SMIL1,SMIL2,SMIL3,SMIL4,ALB_SNOW,W_SNOW,T_SNOW data_in.nc dataland_in.nc

    # -----------------------------------------------------------------------
    # -- Remap land and ocean area differently for variables
    # -----------------------------------------------------------------------

    # -- Ocean part
    # -----------------
    cdo div dataland_in.nc oceanmask_in.nc tmp1_land.nc
    cdo div datasea_in.nc oceanmask_in.nc tmp1_sea.nc

    cdo setmisstodis tmp1_land.nc tmp2_land.nc
    cdo setmisstodis tmp1_sea.nc tmp2_sea.nc

    cdo remap,triangular-grid.nc,weights.nc tmp2_land.nc tmp3_land.nc
    cdo remap,triangular-grid.nc,weights.nc tmp2_sea.nc tmp3_sea.nc

    cdo div tmp3_land.nc oceanmask_out.nc dataland_ocean_out.nc
    cdo div tmp3_sea.nc oceanmask_out.nc datasea_ocean_out.nc

    rm tmp*.nc

    # -- Land part
    # -----------------
    cdo div dataland_in.nc landmask_in.nc tmp1.nc
    cdo setmisstodis tmp1.nc tmp2.nc
    cdo remap,triangular-grid.nc,weights.nc tmp2.nc tmp3.nc
    cdo div tmp3.nc landmask_out.nc dataland_land_out.nc
    rm tmp*.nc dataland_in.nc datasea_in.nc

    # -- Merge remapped land and ocean part
    # --------------------------------------
    cdo ifthenelse LSM_out.nc dataland_land_out.nc dataland_ocean_out.nc dataland_out.nc
    rm dataland_ocean_out.nc dataland_land_out.nc

    # -- Remap the rest (everything not already handled above) and merge
    # -- all files
    # --------------------------------------
    ncks -O -h -x -v SKT,STL1,STL2,STL3,STL4,SMIL1,SMIL2,SMIL3,SMIL4,ALB_SNOW,W_SNOW,T_SNOW,SST,CI,LSM data_in.nc datarest_in.nc
    cdo -s remap,triangular-grid.nc,weights.nc datarest_in.nc era5_final.nc
    rm datarest_in.nc

    # -- Fill NaN values for SST and CI
    cdo setmisstodis -selname,SST,CI datasea_ocean_out.nc dataland_ocean_out_filled.nc
    rm datasea_ocean_out.nc

    # -- Merge remapped files plus land-sea mask from EXTPAR
    ncks -h -A dataland_out.nc era5_final.nc
    ncks -h -A dataland_ocean_out_filled.nc era5_final.nc
    ncks -h -A -v FR_LAND LSM_out.nc era5_final.nc
    ncrename -h -v FR_LAND,LSM era5_final.nc
    rm dataland_out.nc dataland_ocean_out_filled.nc

    # --------------------------------------
    # -- Create the LNPS variable
    # --------------------------------------
    # -- Computed after remapping, from the remapped PS, so that
    # -- LNPS = ln(PS) is consistent with the PS in this file (ln() and
    # -- remapping don't commute).
    cdo expr,'LNPS=ln(PS)' era5_final.nc tmp.nc
    ncks -A -v LNPS tmp.nc era5_final.nc
    rm tmp.nc

    # ---------------------------------
    # -- Post-processing
    # ---------------------------------
    ncrename -h -d .cell,ncells -d .nv,vertices era5_final.nc
    if ! ncks -m -v clon_bnds era5_final.nc >/dev/null 2>&1; then
        ncks -h -A -v clon_bnds,clat_bnds grid_bnds.nc era5_final.nc
    fi

    # -- Set the time of this timestep explicitly: LSM_out.nc, the first
    # -- argument of `cdo ifthenelse` above, carries the time of
    # -- input_files[0], which cdo can pass on to the output.
    cdo -s settaxis,"${{timestamp:0:4}}-${{timestamp:4:2}}-${{timestamp:6:2}}","${{timestamp:8:2}}:00:00" era5_final.nc era5_final_taxis.nc
    mv era5_final_taxis.nc era5_final.nc

    ncks -O era5_final.nc "$inicond_filename"
    rm era5_final.nc data_in.nc

    echo "Saved $inicond_filename"

done

# -- Clean up the one-time grid/weight/mask files
rm -f weights.nc triangular-grid.nc grid_bnds.nc oceanmask_in.nc landmask_in.nc oceanmask_out.nc landmask_out.nc LSM_out.nc

{cfg.cdo_nco_cmd_post}
