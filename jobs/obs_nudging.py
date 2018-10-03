#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging
import shutil

from . import tools

def main(starttime,hstart,hstop,cfg):
    """Copy photolysis-rate file to the **COSMO** input directory.

    Copy the photolysis-rate file from the project (``cfg.photo_rate_file``) to
    the **COSMO** input folder on scratch (``cfg.cosmo_input/art_photolysis``).

    Parameters
    ----------	
    start_time : datetime-object
        The starting date of the simulation
    hstart : int
        Offset (in hours) of the actual start from the start_time
    hstop : int
        Length of simulation (in hours)
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """
    tools.check_target(cfg, 'cosmoart')

    print("Should be copying obs_nudging data to cosmo_input, but don't know which filename is expected from cosmo")



# copy to cosmoart_processing_chain/example_cosmoart/2015_02_04_0_3/cosmo/input
