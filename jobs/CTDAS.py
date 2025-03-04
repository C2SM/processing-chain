import logging
import xarray as xr
import re
import subprocess
from . import tools, prepare_icon
import time
import shutil
from datetime import timedelta

BASIC_PYTHON_JOB = False


def submit_job(command):
    """Submit a job and return the job ID."""
    logging.info(f"Running: {command}")
    result = subprocess.run(command,
                            shell=True,
                            capture_output=True,
                            text=True,
                            check=False)
    match = re.search(r"Submitted batch job (\d+)", result.stdout)

    if match:
        return match.group(1)

    logging.error("Failed to get job ID from sbatch output.")
    return None


def wait_for_job(job_id):
    """Wait for a job to complete."""
    if not job_id:
        return False, None

    logging.info(f"Waiting for job {job_id} to complete...")
    while True:
        result = subprocess.run(f"sacct -j {job_id} --format=State --noheader",
                                shell=True,
                                capture_output=True,
                                text=True)
        state = result.stdout.strip()

        if state:
            logging.info(f"Job {job_id} state: {state}")
            if any(s in state
                   for s in ["COMPLETED", "FAILED", "CANCELLED", "TIMEOUT"]):
                logging.info(f"Job {job_id} finished with state: {state}")
                return False, state
            if any(s in state for s in [
                    "COMPLETED",
            ]):
                logging.info(f"Job {job_id} finished with state: {state}")
                return True, state
        time.sleep(10)


def run_icon_case(cfg, suffix="", output_file=None, max_retries=5):
    """Run an ICON case job and wait for it to complete if output is not already present."""
    if output_file and output_file.exists():
        logging.info(
            f"Skipping ICON case {suffix} as output exists: {output_file}")
        return True

    icon_ini_template = cfg.case_path / cfg.icon_runjob_filename
    job_name = f"{icon_ini_template.stem}_{cfg.startdate_sim.strftime('%Y%m%d')}{suffix}"
    icon_ini_job = cfg.icon_work / (job_name + icon_ini_template.suffix)

    command = f"uenv run icon-wcp -- sbatch {icon_ini_job} --wait"
    logging.info(f"Running ICON case job with {command}")
    logging.info(f"To generate {output_file}")

    retries = 0
    while retries <= max_retries:
        job_id = submit_job(command)
        completed, state = wait_for_job(job_id)

        if completed:
            return True

        if state in ["FAILED", "CANCELLED", "TIMEOUT"]:
            retries += 1
            logging.warning(
                f"Job failed with state {state}. Retrying {retries}/{max_retries}..."
            )
        else:
            break

    logging.error("ICON job failed after maximum retries.")
    return False


def start_ctdas(cfg):
    """Start CTDAS process."""
    logging.info("Starting CTDAS")
    try:
        command = f"cd {cfg.CTDAS_ctdas_path} && ./start_ctdas.sh $SCRATCH ctdas_procchain"
        subprocess.run(command, shell=True, check=True)
        command = "cd $SCRATCH/ctdas_procchain/exec && sbatch ctdas_procchain.jb"
        subprocess.run(command, shell=True, check=True)
    except subprocess.CalledProcessError:
        logging.info(
            "CTDAS already exists -- we did NOT instantiate this CTDAS run")


def main(cfg):
    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)
    """
    Start CTDAS inversion

    This does the following steps:
    1. Run the first day (spin-up)
    2. Start CTDAS

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    global_output_path = cfg.case_root / "global_outputs"
    output_file_1 = global_output_path / f"opt2_{(cfg.startdate_sim - timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y%m%d')}" / f"ICON-ART-OEM-INIT_{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time)).strftime('%Y-%m-%dT%H:%M:%S')}.000.nc"
    output_file_2 = global_output_path / f"runthrough_{(cfg.startdate_sim - timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y%m%d')}" / f"ICON-ART-OEM-INIT_{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time)).strftime('%Y-%m-%dT%H:%M:%S')}.000.nc"
    output_file_3 = global_output_path / f"runthrough_{(cfg.startdate_sim).strftime('%Y%m%d')}" / f"ICON-ART-OEM-INIT_{(cfg.startdate_sim + timedelta(seconds=cfg.CTDAS_restart_init_time) + timedelta(days=cfg.CTDAS_ctdas_cycle)).strftime('%Y-%m-%dT%H:%M:%S')}.000.nc"

    if cfg.startdate_sim == cfg.startdate:
        logging.info("Prepare CTDAS for global simulations")

        logging.info("Run first ICON case")
        run_icon_case(cfg, output_file=output_file_1)

        logging.info("Start CTDAS")
        start_ctdas(cfg)

        if cfg.CTDAS_runthrough:
            run_icon_case(cfg,
                          "_firstrun_runthrough",
                          output_file=output_file_2)

    if cfg.CTDAS_runthrough:
        run_icon_case(cfg, "_runthrough", output_file=output_file_3)

    logging.info("OK")
    shutil.copy(cfg.logfile, cfg.logfile_finish)
