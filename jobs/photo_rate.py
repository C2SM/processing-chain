#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging

from . import tools


BASIC_PYTHON_JOB = True


def main(cfg):
    """Copy photolysis-rate file to the **COSMOART** input directory.

    Only necessary for **COSMOART** simulations.

    Copy the photolysis-rate file from the project (`cfg.photo_rate_file`) to
    the **COSMOART** input folder on scratch (`cfg.cosmo_input/art_photolysis`).

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    tools.change_logfile(cfg.logfile)
    tools.check_model(cfg, 'cosmo-art')
    launch_time = cfg.init_time_logging("photo_rate")

    logging.info("Copying photolysis-rate file from {} to {}".format(
        cfg.photo_rate_file,
        os.path.join(cfg.cosmo_input, "art_photolysis", "papa_data.p")))

    tools.create_dir(os.path.join(cfg.cosmo_input, "art_photolysis"),
                     "photolysis rate input")

    src_file = cfg.photo_rate_file
    dest_path = os.path.join(cfg.cosmo_input, 'art_photolysis', 'papa_data.d')

    tools.copy_file(src_file, dest_path)

    logging.info("Finished")

    cfg.finish_time_logging("photo_rate", launch_time)
