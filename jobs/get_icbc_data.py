#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import shutil
from datetime import timedelta
from pathlib import Path

from . import tools
from .tools.fetch_external_data import (fetch_era5_arco, fetch_cams_co2_months,
                                        icon_grid_bounds)

BASIC_PYTHON_JOB = False


def _year_months(start_date, end_date):
    """List of (year, month) pairs covering [start_date, end_date]."""
    year_months = []
    cursor = start_date.replace(day=1)
    end_marker = end_date.replace(day=1)
    while cursor <= end_marker:
        year_months.append((cursor.year, cursor.month))
        cursor = (cursor + timedelta(days=32)).replace(day=1)
    return year_months


def main(cfg):
    """
    **Fetch ICBC Data**

    Downloads ERA5 (from the public Google ARCO Zarr store -- no CDS
    credentials needed) and CAMS CO2 (via ADS, restricted to the month(s)
    the current chunk falls in) data for use as ICON initial/boundary
    conditions.

    Deliberately does *not* set BASIC_PYTHON_JOB, so it runs directly on the
    login node instead of being shipped off to SLURM: both fetches spend
    most of their time waiting on a remote service, not doing local compute,
    so there's no reason to occupy a compute node for it.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    tools.change_logfile(cfg.logfile)
    logging.info("Fetch ICBC data (ERA5 / CAMS)")

    fetch_start = cfg.startdate_sim
    fetch_end = cfg.enddate_sim + timedelta(days=1)

    if getattr(cfg, 'meteo_fetch_era5', False):
        era5_dir = cfg.case_root / "global_inputs" / "ERA5"
        tools.create_dir(era5_dir, "ERA5 input files")
        times = list(tools.iter_hours(fetch_start, fetch_end, cfg.meteo_inc))
        grid_file = Path(cfg.input_files['dynamics_grid_filename'])
        if not grid_file.is_absolute():
            grid_file = (cfg.case_path / grid_file).resolve()
        area = icon_grid_bounds(grid_file)
        logging.info(f"ERA5 area (latmin, latmax, lonmin, lonmax): {area}")
        fetch_era5_arco(times, era5_dir, area=area)

    if getattr(cfg, 'chem_fetch_cams', False):
        cams_dir = cfg.case_root / "global_inputs" / "CAMS"
        tools.create_dir(cams_dir, "CAMS input files")
        year_months = _year_months(fetch_start, fetch_end)
        fetch_cams_co2_months(year_months,
                              cams_dir,
                              start_date=fetch_start,
                              end_date=fetch_end)

    logging.info("OK")

    # Not a BASIC_PYTHON_JOB, so run_chain.py's own "mark finished" step
    # (shutil.copy(cfg.logfile, cfg.logfile_finish)) never runs for this
    # job -- that only happens for the nested/force_sync invocation
    # BASIC_PYTHON_JOB=True jobs get shipped off to. Do it here instead,
    # so a re-run of this chunk doesn't needlessly re-fetch.
    shutil.copy(cfg.logfile, cfg.logfile_finish)
