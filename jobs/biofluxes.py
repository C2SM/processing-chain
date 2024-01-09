#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
from . import tools, prepare_icon


def main(cfg):
    """Prepare biofluxes files for COSMO simulations.

    Copies biofluxes files from the project folder (:attr:`cfg.vprm['dir']`)
    to the int2lm input folder on scratch (:attr:`cfg.int2lm_input`/vprm).

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    tools.check_model(cfg, 'cosmo-ghg')
    prepare_icon.set_cfg_variables(cfg)

    scratch_path = os.path.join(cfg.int2lm_input, 'vprm')

    tools.create_dir(scratch_path, "biofluxes input")

    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim):
        logging.info(time)

        for prefix in cfg.vprm['prefix']:
            filename = os.path.join(cfg.vprm['dir'],
                                    prefix + time.strftime('%Y%m%d%H.nc'))
            filename_sc = os.path.join(scratch_path,
                                       prefix + time.strftime('%Y%m%d%H.nc'))
            if not (os.path.isfile(filename)):
                logging.error(
                    "File %s not found. Consider using the vprmsplit.py script prior",
                    filename)
                #tools.vprmsplit.main(time.strftime("%Y%m%d%H"),cfg.vprm['dir']_orig,cfg.vprm['dir']_proc,cfg)

            tools.copy_file(filename, scratch_path)

            if not os.path.isfile(filename_sc):
                logging.error(
                    "Splitting or copying of GPP or/and RA files to scratch failed."
                )
