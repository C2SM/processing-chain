#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import os
import shutil
import subprocess
from datetime import timedelta
from pathlib import Path

from . import tools, prepare_icon

BASIC_PYTHON_JOB = False


def run_on_login_node(cfg, scripts):
    """Run the rendered `scripts` one after the other on the login node,
    in the uenv ``remap_icbc.uenv`` if one is configured and the chain is
    not already running in a uenv."""
    command = []
    uenv = getattr(cfg, 'remap_icbc_uenv', None)
    if uenv:
        active = os.environ.get('UENV_LABEL') or (
            'a uenv' if os.environ.get('UENV_MOUNT_LIST') else None)
        if active is None:
            command = ['uenv', 'run', uenv]
            if view := getattr(cfg, 'remap_icbc_view', None):
                command.append(f'--view={view}')
            command.append('--')
        elif active != uenv:
            logging.warning(
                f"remap_icbc.uenv is {uenv}, but the chain runs in {active}; "
                "uenvs cannot be nested, so the scripts use the tools of "
                f"{active}")
    for script in scripts:
        logging.info(f"Running {script}")
        with open(cfg.logfile, 'a') as log:
            subprocess.run(command + ['bash', str(script)],
                           cwd=script.parent,
                           stdout=log,
                           stderr=subprocess.STDOUT,
                           check=True)


def main(cfg):
    """
    **Remap ICBC Data**

    Runs the case's ICBC remapping scripts, in order, as one SLURM job, or
    directly on the login node with ``remap_icbc.login_node: True``.
    Which scripts run -- and what they do (remap ERA5 onto the ICON grid,
    fold in one or more CAMS species, or anything else a case needs) -- is
    entirely up to the case: list them under ``remap_icbc.scripts`` in
    config.yaml, as paths relative to the case directory, e.g.::

        remap_icbc:
            scripts:
                - ICBC/remap_era5.sh
                - ICBC/add_cams_co2.sh
            uenv: netcdf-tools/2025:v1   # optional
            view: netcdf                 # optional
            login_node: False            # optional

    Each script is rendered with ``.format(cfg=cfg, era5_dir=..., cams_dir=
    ..., outdir=..., workdir=..., era5_timestamps=...)`` before being run,
    so it can use whichever of those placeholders (plus any ``cfg.xxx``
    attribute) it needs -- e.g. a case fetching several CAMS species could
    list one script per species, or a single script handling all of them.

    The tools the scripts need (CDO, NCO, ...) can come from a uenv
    (``remap_icbc.uenv`` and ``remap_icbc.view``, Santis only), or from shell
    lines that the scripts wrap their commands in: ``{cfg.cdo_nco_cmd}`` ...
    ``{cfg.cdo_nco_cmd_post}``. All of these default to empty. On the login
    node, the uenv is only started if the chain is not already running in
    one (uenvs cannot be nested); otherwise the active uenv is used.

    The time limit of the SLURM job is ``walltime.remap_icbc`` (default
    00:30:00).

    In a restart chunk, the initial file of the experiment start date is
    linked from the previous chunk, because ICON opens it in every run.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    prepare_icon.set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)
    logging.info("Remap ICBC data")

    cfg.cdo_nco_cmd = getattr(cfg, 'cdo_nco_cmd', '')
    cfg.cdo_nco_cmd_post = getattr(cfg, 'cdo_nco_cmd_post', '')

    # ICON opens the input file of the experiment start date in every run,
    # restarts included, so link it from the previous chunk
    if cfg.lrestart == '.TRUE.':
        tools.create_dir(cfg.icon_input_icbc, "icon_input_icbc")
        inidata_file = prepare_icon.get_inidata_filename(cfg)
        tools.symlink_file(cfg.icon_input_icbc_prev / inidata_file.name,
                           inidata_file)

    scripts = getattr(cfg, 'remap_icbc_scripts', [])
    if not scripts:
        logging.info("No remap_icbc.scripts configured, nothing to do")
        shutil.copy(cfg.logfile, cfg.logfile_finish)
        return

    workdir = cfg.chain_root / "remap_icbc_work"
    tools.create_dir(workdir, "remap_icbc work directory")

    # The timesteps of this chunk, i.e. the ones get_icbc_data fetched for
    # it. global_inputs/ERA5 holds the data of all chunks.
    fetch_start = cfg.startdate_sim
    fetch_end = cfg.enddate_sim + timedelta(days=1)
    era5_timestamps = ' '.join(
        t.strftime('%Y%m%d%H')
        for t in tools.iter_hours(fetch_start, fetch_end, cfg.meteo_inc))

    kwargs = dict(
        cfg=cfg,
        era5_dir=cfg.case_root / "global_inputs" / "ERA5",
        cams_dir=cfg.case_root / "global_inputs" / "CAMS",
        outdir=cfg.icon_input_icbc,
        workdir=workdir,
        era5_timestamps=era5_timestamps,
    )

    # Render the scripts
    rendered = []
    for script_rel in scripts:
        template = cfg.case_path / script_rel
        rendered_script = workdir / Path(script_rel).name
        rendered_script.write_text(template.read_text().format(**kwargs))
        rendered.append(rendered_script)

    if getattr(cfg, 'remap_icbc_login_node', False):
        run_on_login_node(cfg, rendered)
        logging.info("OK")
        shutil.copy(cfg.logfile, cfg.logfile_finish)
        return

    # Job script running them in order
    walltime = getattr(cfg, 'walltime', {}).get('remap_icbc', '00:30:00')
    job_name = (f"remap_icbc_{cfg.casename}_{cfg.startdate_sim_yyyymmddhh}_"
                f"{cfg.enddate_sim_yyyymmddhh}")
    if cfg.machine == 'santis':
        script_lines = [
            '#!/usr/bin/env bash',
            f'#SBATCH --job-name="{job_name}"',
            f'#SBATCH --account={cfg.compute_account}',
            f'#SBATCH --time={walltime}',
            f'#SBATCH --partition={cfg.compute_queue}',
            f'#SBATCH --constraint={cfg.constraint}',
            '#SBATCH --nodes=1',
        ]
        if uenv := getattr(cfg, 'remap_icbc_uenv', None):
            script_lines.append(f'#SBATCH --uenv={uenv}')
            if view := getattr(cfg, 'remap_icbc_view', None):
                script_lines.append(f'#SBATCH --view={view}')
    else:
        script_lines = [
            '#!/usr/bin/env bash',
            f'#SBATCH --job-name="{job_name}"',
            f'#SBATCH --time={walltime}',
            f'#SBATCH --constraint={cfg.constraint}',
            '#SBATCH --ntasks=1',
        ]
    script_lines += [
        f'#SBATCH --output={cfg.logfile}',
        '#SBATCH --open-mode=append',
        f'#SBATCH --chdir={workdir}',
        '',
        'set -euo pipefail',
    ]
    script_lines += [f'bash {script}' for script in rendered]
    # Mark the job as finished, so that a re-run of this chunk skips it
    script_lines.append(f'cp {cfg.logfile} {cfg.logfile_finish}')

    job_script = workdir / 'run_remap_icbc.job'
    job_script.write_text('\n'.join(script_lines) + '\n')
    cfg.submit('remap_icbc', job_script)
