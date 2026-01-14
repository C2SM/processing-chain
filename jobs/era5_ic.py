#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
era5_ic.py
Processing-chain job that generates ICON initial conditions (IC) from ERA5 input.

Design goals
------------
- Keep the job modular:
  * all paths and filenames come from cfg / case templates
  * the actual transformation is executed in a Slurm job script produced
    from a case-provided template (similar to icontools runjobs)
- Support global runs:
  * generate a single IC file on the ICON grid
  * do NOT generate LBC files
"""

import logging
from pathlib import Path
from . import tools, prepare_icon
BASIC_PYTHON_JOB = True

def _compute_inidata_filename(cfg) -> Path:
    """
    Replicate the same IC filename logic used by jobs/icon.py,
    so that era5_ic produces *exactly* the file ICON will read.
    """
    if hasattr(cfg, 'inicond_filename'):
        return cfg.icon_input_icbc / cfg.inicond_filename
    if (hasattr(cfg, 'inidata_prefix') and hasattr(cfg, 'inidata_nameformat')
            and hasattr(cfg, 'inidata_filename_suffix')):
        return cfg.icon_input_icbc / str(
            cfg.startdate.strftime(cfg.inidata_prefix +
                                   cfg.inidata_nameformat +
                                   cfg.inidata_filename_suffix))
    # fallback: match icon.py default behavior
    return cfg.icon_input_icbc / str(
        cfg.startdate_sim.strftime(cfg.meteo['prefix'] +
                                   cfg.meteo['nameformat']) + '.nc')

def main(cfg):
    """
    1) Prepare standard ICON paths (same helper as other jobs)
    2) Create a Slurm script from the case template cfg.era5_ic_runjob_filename
    3) Submit it
    The Slurm script is responsible for:
    - converting ERA5 GRIB -> NetCDF
    - renaming variables to ICON-like naming (via a partab)
    - remapping to the ICON triangular grid
    - writing the final IC file to cfg.icon_input/icbc
    """

    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)
    logging.info("Generate global ICON initial conditions from ERA5 (IC only).")

    # Ensure run + icbc directories exist (prepare_icon usually created them,
    # but being explicit makes the job robust if invoked in isolation).
    tools.create_dir(cfg.icon_work, "icon_work")
    tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")

    # Useful formatted dates for the template (avoid bash date gymnastics)
    cfg.era5_ymd = cfg.startdate_sim.strftime('%Y-%m-%d')   # e.g. 2021-01-01
    cfg.era5_yyyymmddhh = cfg.startdate_sim.strftime('%Y%m%d%H')  # 2021010100

    # Compute the *exact* file that ICON will later read
    inidata_filename = _compute_inidata_filename(cfg)

    # Case template name (kept configurable)
    # Put in config.yaml: era5_ic_runjob_filename: era5_ic_runjob.cfg
    template_name = getattr(cfg, 'era5_ic_runjob_filename', 'era5_ic_runjob.cfg')
    template = (cfg.case_path / template_name).read_text()
    script_str = template.format(
        cfg=cfg,
        inidata_filename=inidata_filename,
    )
    script = (cfg.icon_work / 'run_era5_ic.job')
    script.write_text(script_str)
    logging.info(f"Submitting ERA5 IC generation job: {script}")
    cfg.submit('era5_ic', script)
    logging.info("OK")
