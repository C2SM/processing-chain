#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copy cosmo output from scratch to store (or anywhere else)

### DEVELOPMENT VERSION ###

import logging
import os
import shutil
import datetime
import glob
from subprocess import call

from . import tools


def logfile_header_template():
    """Returns a template for the logfile-header"""
    return ("\n=====================================================\n"
            "============== POST PROCESSING {}\n"
            "============== {}\n"
            "=====================================================\n\n")


def runscript_header_template():
    """Returns a template for the runscript-header (#SBATCH-directives)"""
    return '\n'.join([
        "#SBATCH --job-name=post_cosmo", "#SBATCH --partition=xfer",
        "#SBATCH --constraint={constraint}",
        "#SBATCH --account={compute_account}", "#SBATCH --output={logfile}",
        "#SBATCH --open-mode=append", "#SBATCH --chdir={cosmo_work}",
        "#SBATCH --time=00:30:00", "", ""
    ])


def runscript_commands_template():
    """Return a template for the commands in the runscript.

    To ensure the bash-commands have the intended behaviour, make sure to strip
    trailing slashes from the directory names (they are added as needed in the
    template).
    """
    commands = list()

    return '\n'.join([
        "srun cp -Raf {int2lm_work_src}/. {int2lm_work_dest}/",
        "srun cp -Raf {cosmo_work_src}/. {cosmo_work_dest}/",
        "srun cp -Raf {cosmo_output_src}/. {cosmo_output_dest}/",
        "srun cp -Raf {logs_src}/. {logs_dest}/"
    ])


def main(starttime, hstart, hstop, cfg, model_cfg):
    """Copy the output of a **COSMO**-run to a user-defined position.

    Write a runscript to copy all files (**COSMO** settings & output,
    **int2lm** settings, logfiles) from ``cfg.cosmo_work``,
    ``cfg.cosmo_output``, ``cfg.int2lm_work``, ``cfg.log_finished_dir`` to
    ``cfg.output_root/...`` .
    If the job ``reduce_output`` has been run before ``post_cosmo``, a 
    directory ``cfg.cosmo_output_reduced`` is created. In this case,
    ``cfg.cosmo_output_reduced`` is copied instead of ``cfg.cosmo_output``.
    
    Submit the job to the xfer-queue.
    
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
    if cfg.compute_host != "daint":
        logging.error("The copy script is supposed to be run on daint only,"
                      "not on {}".format(cfg.compute_host))
        raise RuntimeError("Wrong compute host for copy-script")

    logfile = os.path.join(cfg.log_working_dir, "post_cosmo")
    cosmo_work_dir = cfg.cosmo_work
    runscript_path = os.path.join(cfg.cosmo_work, "post_cosmo.job")
    copy_path = os.path.join(
        cfg.output_root,
        starttime.strftime('%Y%m%d%H') + "_" + str(int(hstart)) + "_" +
        str(int(hstop)))

    logging.info(logfile_header_template().format(
        "STARTS", str(datetime.datetime.today())))

    # Prepare the runscript
    runscript_content = "#!/bin/bash\n"
    runscript_content += runscript_header_template().format(
        compute_account=cfg.compute_account,
        logfile=logfile,
        constraint=cfg.constraint,
        cosmo_work=cfg.cosmo_work)

    if os.path.isdir(cfg.cosmo_output_reduced):
        cosmo_output_src = cfg.cosmo_output_reduced.rstrip('/')
        cosmo_output_dest = os.path.join(copy_path,
                                         "cosmo_output_reduced").rstrip('/')
    else:
        cosmo_output_src = cfg.cosmo_output.rstrip('/')
        cosmo_output_dest = os.path.join(copy_path, "cosmo_output").rstrip('/')

    # Create new directories
    os.makedirs(os.path.join(copy_path, "int2lm_run"), exist_ok=True)
    os.makedirs(os.path.join(copy_path, "cosmo_run"), exist_ok=True)
    os.makedirs(cosmo_output_dest, exist_ok=True)
    os.makedirs(os.path.join(copy_path, "logs"), exist_ok=True)

    # Format the runscript
    runscript_content += runscript_commands_template().format(
        target_dir=copy_path.rstrip('/'),
        int2lm_work_src=cfg.int2lm_work.rstrip('/'),
        int2lm_work_dest=os.path.join(copy_path, "int2lm_run").rstrip('/'),
        cosmo_work_src=cfg.cosmo_work.rstrip('/'),
        cosmo_work_dest=os.path.join(copy_path, "cosmo_run").rstrip('/'),
        cosmo_output_src=cosmo_output_src,
        cosmo_output_dest=cosmo_output_dest,
        logs_src=cfg.log_finished_dir.rstrip('/'),
        logs_dest=os.path.join(copy_path, "logs").rstrip('/'))

    # Wait for Cosmo to finish first
    tools.check_job_completion(cfg.log_finished_dir, "cosmo")

    with open(runscript_path, "w") as script:
        script.write(runscript_content)

    logging.info("Submitting the copy job to the xfer queue")
    logging.info("Make sure you have the module 'xalt' unloaded!")

    sbatch_wait = getattr(cfg, "wait", "True")

    if sbatch_wait:
        exitcode = call(["sbatch", "--wait", runscript_path])
        logging.info(logfile_header_template().format(
            "ENDS", str(datetime.datetime.today())))

        # copy own logfile aswell
        tools.copy_file(logfile, os.path.join(copy_path, "logs/"))

    else:
        exitcode = call(["sbatch", runscript_path])

    if exitcode != 0:
        raise RuntimeError("sbatch returned exitcode {}".format(exitcode))
