# Parent global R2B7 ERA5 case

Global ICON run on an R2B7 grid, initialised from ERA5 (IC only, no LBC):

- stage static ICON inputs (grid/extpar/radiation tables)
- generate ERA5-based initial conditions on R2B7 (`era5_ic`)
- run a global NWP-style ICON simulation
- write hourly evaluation outputs (surface, pressure levels, native 3D)
- write restarts at `restart_step`

## Status: scaffold

This case does **not** run out of the box. Before using it:

1. **Adapt the absolute paths in `config.yaml`.** The ICON binary, the
   radiation tables (`lrtm_filename`, `cldopt_filename`), `map_file_ana`,
   `ecrad_data_path`, `icon_data_path` and `era5_dir` all point into
   `/cluster/work/climate/lroither/...` and must be changed to your own
   installation.
2. **Provide the grid files.** `input/icon/grid/zonda_output_I/` is not part
   of the CI input archive and has to be generated or copied in.
3. **Create the ecrad/Kinne/MACv2/ozone symlinks by hand** in the run
   directory. `load_links.txt` lists the commands; this step is not yet
   automated by any job.
4. **Check `start_year`** — it is used for the year-specific aerosol and
   ozone files and must match the simulation period.
