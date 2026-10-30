# export PATH=$HOME/envs/nctools/bin:$PATH
export PATH=/capstor/scratch/cscs/msteiner/envs/nctools/bin:$PATH
# -- Zielgitter: 14.536.0 °E, 39.550.5 °N, 0.05°
cat > latlon_grid.txt <<EOF
gridtype = lonlat
xsize = 431
ysize = 221
xfirst = 14.5
xinc = 0.05
yfirst = 39.5
yinc = 0.05
EOF
# -- Gewichte einmal berechnen
shopt -s nullglob
files=(ICON-ART-UNSTR_DOM01_*.nc)
(( ${#files[@]} )) || { echo "Keine ICON-ART-UNSTR_DOM01_*.nc gefunden"; exit 1; }
cdo -P 8 gendis,latlon_grid.txt "${files[0]}" weights_latlon.nc
for f in ICON-ART-UNSTR_DOM01_*.nc; do
    cdo -P 8 remap,latlon_grid.txt,weights_latlon.nc "$f" "${f/UNSTR/STR}"
done

