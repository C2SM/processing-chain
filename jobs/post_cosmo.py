#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copy cosmo output from scratch to store (or anywhere else)

### DEVELOPMENT VERSION ###

import logging
import os
import datetime
from subprocess import call

from . import tools, int2lm, cosmo


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
        "#SBATCH --open-mode=append", "#SBATCH --chdir={cosmo_run}",
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
        "srun cp -Raf {int2lm_run_src}/. {int2lm_run_dest}/",
        "srun cp -Raf {cosmo_run_src}/. {cosmo_run_dest}/",
        "srun cp -Raf {cosmo_output_src}/. {cosmo_output_dest}/",
        "srun cp -Raf {logs_src}/. {logs_dest}/"
    ])


def main(cfg):
    """Copy the output of a **COSMO**-run to a user-defined position.

    Write a runscript to copy all files (**COSMO** settings & output,
    **int2lm** settings, logfiles) from ``cfg.cosmo_run``,
    ``cfg.cosmo_output``, ``cfg.int2lm_run``, ``cfg.log_finished_dir`` to
    ``cfg.output_root/...`` .
    If the job ``reduce_output`` has been run before ``post_cosmo``, a 
    directory ``cfg.cosmo_output_reduced`` is created. In this case,
    ``cfg.cosmo_output_reduced`` is copied instead of ``cfg.cosmo_output``.
    
    Submit the job to the xfer-queue.
    
    Parameters
    ----------	
    cfg : config-object
        Object holding all user-configuration parameters as attributes
    """
    cfg = int2lm.set_cfg_variables(cfg)
    cfg = cosmo.set_cfg_variables(cfg)

    logfile = os.path.join(cfg.log_working_dir, "post_cosmo")
    cosmo_run_dir = cfg.cosmo_run
    runscript_path = os.path.join(cfg.cosmo_run, "post_cosmo.job")
    copy_path = os.path.join(
        cfg.post_cosmo['output_root'],
        cfg.startdate_sim_yyyymmddhh + "_" + cfg.enddate_sim_yyyymmddhh)

    logging.info(logfile_header_template().format(
        "STARTS", str(datetime.datetime.today())))

    # Prepare the runscript
    runscript_content = "#!/bin/bash\n"
    runscript_content += runscript_header_template().format(
        compute_account=cfg.compute_account,
        logfile=logfile,
        constraint=cfg.constraint,
        cosmo_run=cfg.cosmo_run)

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

    int2lm_run_path = os.path.abspath(os.path.join(copy_path, "int2lm_run"))
    cosmo_run_path = os.path.abspath(os.path.join(copy_path, "cosmo_run"))
    cosmo_output_dest_path = os.path.abspath(cosmo_output_dest)
    logs_path = os.path.abspath(os.path.join(copy_path, "logs"))

    # Format the runscript
    runscript_content += runscript_commands_template().format(
        target_dir=copy_path.rstrip('/'),
        int2lm_run_src=cfg.int2lm_run.rstrip('/'),
        int2lm_run_dest=int2lm_run_path.rstrip('/'),
        cosmo_run_src=cfg.cosmo_run.rstrip('/'),
        cosmo_run_dest=cosmo_run_path.rstrip('/'),
        cosmo_output_src=cosmo_output_src,
        cosmo_output_dest=cosmo_output_dest_path,
        logs_src=cfg.log_finished_dir.rstrip('/'),
        logs_dest=logs_path.rstrip('/'))

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
