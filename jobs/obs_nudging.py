#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import logging

from datetime import timedelta
from . import tools

BASIC_PYTHON_JOB = True


def main(cfg):
    """Copy and rename the obs_nudging files to the **COSMO** input directory.

    In the folder ``cfg.obs_nudging_dir``, the files are saved in the format
    ``{prefix}-YYYYMMDD000000-YYYYMMDD000000``. **COSMO** expects files in the 
    format ``{prefix}x``, where ``x`` is ``[nothing], .2, .3, .4, ...``. This 
    job handles this filename-change and copies them to the **COSMO** input 
    folder on scratch (``cfg.cosmo_input/obs_nudging``).

    Missing observation files are ignored.

    Also copies the blacklist-file blklsttmp.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    tools.change_logfile(cfg.logfile)
    dest_dir = os.path.join(cfg.cosmo_input, "obs_nudging")
    tools.create_dir(dest_dir, "obs nudging input")

    logging.info("Copying obs_nudging files from {} to {}".format(
        cfg.obs_nudging_dir, dest_dir))

    for i, t in enumerate(
            tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim, step=24)):
        i += 1
        for prefix in cfg.obs_nudging_prefixes:
            src_filename = (
                prefix + t.strftime(cfg.obs_nudging_date_format) +
                (t + timedelta(days=1)).strftime(cfg.obs_nudging_date_format))
            src_path = os.path.join(cfg.obs_nudging_dir, src_filename)
            if i == 1:
                dest_path = os.path.join(dest_dir, prefix)
            else:
                dest_path = os.path.join(dest_dir, "{}.{}".format(prefix, i))

            try:
                tools.copy_file(src_path, dest_path)
            except BlockingIOError:
                continue
            logging.info("Copied file {} to {}".format(src_filename,
                                                       dest_path))

    tools.copy_file(os.path.join(cfg.obs_nudging_dir, 'blklsttmp'), dest_dir)

    logging.info("Copied blacklist-file to {}".format(dest_dir))
