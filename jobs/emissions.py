#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging

from . import tools, prepare_icon


def main(cfg):
    """Copy emission files to the int2lm input directory.

    Necessary for both COSMO and COSMOART simulations.

    Copy emission files from project folder (``cfg.emissions['dir']``) to
    int2lm input folder on scratch (``cfg.int2lm_input/emissions``).

    For COSMO simulations, converts the netCDF-variable-names 
    from ``string`` to ``char`` (necessary for int2lm).

    If there are multiple emission-datasets (cfg.emissions['dir'] is a list of
    paths), they are copied as follows::

        cfg.emissions['dir'][0]/cfg.emissions['gridname'][0]YYYYMMDD.nc -> int2lm_input/emissions/emis_YYYYMMDD.nc
        cfg.emissions['dir'][1]/cfg.emissions['gridname'][1]YYYYMMDD.nc -> int2lm_input/emissions2/emis_YYYYMMDD.nc
        cfg.emissions['dir'][2]/cfg.emissions['gridname'][2]YYYYMMDD.nc -> int2lm_input/emissions3/emis_YYYYMMDD.nc

    Parameters
    ----------	
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    prepare_icon.set_cfg_variables(cfg)
    dest_prefix = "emis_"

    if not isinstance(cfg.emissions['dir'], list):
        emis_dirs = [cfg.emissions['dir']]
    else:
        emis_dirs = cfg.emissions['dir']
    if not isinstance(cfg.emissions['gridname'], list):
        emis_prefixes = [cfg.emissions['gridname']]
    else:
        emis_prefixes = cfg.emissions['gridname']

    assert len(emis_dirs) == len(emis_prefixes), (
        "Different number of cfg.emissions['dir'] and cfg.emissions['gridname']"
    )

    for i, (emis_dir, emis_prefix) in enumerate(zip(emis_dirs, emis_prefixes)):
        # create directory
        if i == 0:
            target_dir = os.path.join(cfg.int2lm_input, "emissions")
            tools.create_dir(target_dir, "emissions input")
        else:
            target_dir = os.path.join(cfg.int2lm_input,
                                      "emissions" + str(i + 1))
            tools.create_dir(target_dir, "emissions input")
        # copy data
        for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim):
            logging.info(time)
            filename_ending = time.strftime('%Y%m%d%H.nc')
            source_path = os.path.join(emis_dir, emis_prefix + filename_ending)
            dest_path = os.path.join(target_dir, dest_prefix + filename_ending)
            tools.copy_file(source_path, dest_path)

            # convert grid_mapping_name from string (NF90_STRING) to char
            # (NF90_CHAR) (needed for int2lm to work)
            if cfg.workflow_name.startswith('cosmo'):
                tools.string2char.main(dest_path)
