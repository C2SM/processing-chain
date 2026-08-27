# Parent global R2B7 ERA5 case

Global ICON run on an R2B7 grid, initialised from ERA5 (IC only, no LBC):

- stage static ICON inputs (grid/extpar/radiation tables)
- generate ERA5-based initial conditions on R2B7 (`era5_ic`)
- run a global NWP-style ICON simulation
- write hourly evaluation outputs (surface, pressure levels, native 3D)
- write restarts at `restart_step`

## Status: scaffold

This case does **not** run out of the box. Before using it:

1. **Provide the ERA5 input.** `era5_dir` still points into
   `/cluster/work/climate/lroither/...` and must be changed to your own
   ERA5 GRIB collection. The ICON binary and its data files come from the
   provided install at
   `/cluster/work/climate/icon_input/icon-model/release-2026.04-public`.
2. **Provide the grid files.** `input/icon/grid/zonda_output_I/` is not part
   of the CI input archive and has to be generated or copied in.
3. **Create the ecrad/Kinne/MACv2/ozone symlinks by hand** in the run
   directory. `load_links.txt` lists the commands; this step is not yet
   automated by any job. Note that the Kinne, CMIP6 volcanic and ozone
   boundary conditions are *not* part of the provided ICON install and
   still come from a separate tree.
4. **Check `start_year`** — it is used for the year-specific aerosol and
   ozone files and must match the simulation period.
