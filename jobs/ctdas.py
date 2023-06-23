#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import logging
import os
import subprocess
from .tools import write_cosmo_input_ghg
from . import tools
from datetime import datetime, timedelta


def main(starttime, hstart, hstop, cfg):

    logging.info("Setup the namelist for an CTDAS ICON run and "
                 "submit the jobs to the queue")

    # Copy icon executable
    execname = 'icon.exe'
    tools.copy_file(cfg.icon_bin, os.path.join(cfg.icon_work, execname))

    cfg.ctdas_exec = os.path.join(cfg.ctdas_root, 'exec')
    cfg.ctdas_rc = os.path.join(cfg.ctdas_exec, 'ctdas-icon.rc')
    cfg.ctdas_job = os.path.join(cfg.ctdas_exec, 'ctdas-icon.jb')
    cfg.ctdas_sys_rc = os.path.join(cfg.ctdas_exec, 'da', 'rc', 'cteco2',
                                    'carbontracker_icon.rc')
    cfg.ctdas_case_rc = os.path.join(
        cfg.ctdas_exec, 'ctdas-icon-' + cfg.ini_datetime_string + '.rc')
    cfg.ctdas_case_sys_rc = os.path.join(
        cfg.ctdas_exec, 'da', 'rc', 'cteco2',
        'carbontracker_icon_' + cfg.ini_datetime_string + '.rc')
    cfg.ctdas_rc = os.path.join(cfg.ctdas_exec, 'ctdas-icon.rc')
    cfg.ctdas_ini_datetime_string = cfg.ini_datetime_string.replace(
        'T', ' ').replace('Z', '')
    cfg.ctdas_end_datetime_string = cfg.end_datetime_string.replace(
        'T', ' ').replace('Z', '')

    # Write ctdas_job.rc file
    with open(cfg.ctdas_rc) as input_file:
        to_write = input_file.read()
    output_file = os.path.join(cfg.ctdas_case_rc)
    with open(output_file, "w") as outf:
        outf.write(to_write.format(cfg=cfg))

    # Write ctdas_system.rc file
    with open(cfg.ctdas_sys_rc) as input_file:
        to_write = input_file.read()
    output_file = os.path.join(cfg.ctdas_case_sys_rc)
    with open(output_file, "w") as outf:
        outf.write(to_write.format(cfg=cfg))

    # Write ctdas.job file
    with open(cfg.ctdas_job) as input_file:
        to_write = input_file.read()
    output_file = os.path.join(cfg.ctdas_root, 'exec',
                               'ctdas-icon-' + cfg.ini_datetime_string + '.jb')
    with open(output_file, "w") as outf:
        outf.write(to_write.format(cfg=cfg))

    #Create dir for extracted data
    tools.create_dir(os.path.join(cfg.icon_base, "extracted"),
                     "extracted data")

    # Run CTDAS
    exitcode = subprocess.call([
        "sbatch", "--wait",
        os.path.join(cfg.ctdas_root, 'exec',
                     'ctdas-icon-' + cfg.ini_datetime_string + '.jb')
    ])

    if exitcode != 0:
        raise RuntimeError("sbatch returned exitcode {}".format(exitcode))
