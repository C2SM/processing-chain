# ICON global R2B7 ERA5 case

Global ICON run on an R2B7 grid, initialised from ERA5 (IC only, no LBC):

- stage static ICON inputs (grid/extpar/radiation tables)
- generate ERA5-based initial conditions on R2B7 (`era5_ic`)
- run a global NWP-style ICON simulation
- write hourly evaluation outputs (surface, pressure levels, native 3D)
- write restarts at `restart_step`

## Status: scaffold

This case does **not** run out of the box. Before using it:

1. ~~**Provide the ERA5 input.**~~ Done. `era5_dir` points to
   `/cluster/work/climate/icon_testing_input/processing_chain/era5/raw`,
   which holds the `era5_ml_2013-05-25.grib` / `era5_surf_2013-05-25.grib`
   files this case's init date needs (copied from lroither's collection).
   The ICON binary and its data files come from the provided install at
   `/cluster/work/climate/icon_input/icon-model/release-2026.04-public`.
2. ~~**Provide the grid files.**~~ Done. `I_DOM01.nc`, `I_DOM01.parent.nc`
   and `I_DOM01_external_parameter.nc` were copied (from lroither's
   `zonda_output_I` grid generation output) to
   `/cluster/work/climate/icon_testing_input/processing_chain/icon/grid/zonda_output_I/`,
   and `input_files` in `config.yaml` points there.
3. ~~**Create the ecrad/Kinne/MACv2/ozone symlinks by hand**~~ Done for the
   current chunk's run directory (`load_links.txt` has the commands). This
   step is not yet automated by any job, so it must be repeated for every
   new chunk directory `prepare_icon` creates. The Kinne (coa/fin), CMIP6
   volcanic and ozone (ssp370, 2013) boundary conditions are *not* part of
   the provided ICON install; they were copied to
   `/cluster/work/climate/icon_testing_input/processing_chain/ecrad_bc/`
   (~21.7 GB, from lroither's tree — no matching resolution/scenario was
   found under `icon_input`, except the CMIP6 volcanic file, which is
   byte-identical to the one already in `icon_input` and was copied from
   there instead). MACv2 continues to come from the provided ICON install.
4. **Check `start_year`** — it is used for the year-specific aerosol and
   ozone files and must match the simulation period.
