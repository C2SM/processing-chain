#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging

from . import tools


def main(cfg, model_cfg):
    """Copy photolysis-rate file to the **COSMOART** input directory.

    Only necessary for **COSMOART** simulations.

    Copy the photolysis-rate file from the project (`cfg.photo_rate_file`) to
    the **COSMOART** input folder on scratch (`cfg.cosmo_input/art_photolysis`).

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    model_cfg : dict
        Model configuration settings loaded from the ``config/models.yaml`` file.
    """
    tools.check_model(cfg, 'cosmo-art')

    logging.info("Copying photolysis-rate file from {} to {}".format(
        cfg.photo_rate_file,
        os.path.join(cfg.cosmo_input, "art_photolysis", "papa_data.p")))

    tools.create_dir(os.path.join(cfg.cosmo_input, "art_photolysis"),
                     "photolysis rate input")

    src_file = cfg.photo_rate_file
    dest_path = os.path.join(cfg.cosmo_input, 'art_photolysis', 'papa_data.d')

    tools.copy_file(src_file, dest_path)

    logging.info("Finished")
