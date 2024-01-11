#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pathlib import Path
import logging
from datetime import timedelta
from . import tools

BASIC_PYTHON_JOB = True


def set_cfg_variables(cfg):
    cfg.int2lm_root = cfg.chain_root / 'int2lm'
    cfg.int2lm_input = cfg.int2lm_root / 'input'


def main(cfg):
    """
    **COSMO Data Preparation**

    This function prepares input data for COSMO simulations by creating necessary directories,
    copying meteorological files, and handling specific data processing.

    - Copy meteorological files to **int2lm** input.
    - Create the necessary directory ``cfg.int2lm_input/meteo``.
    - Copy meteorological files from the project directory (``cfg.meteo['dir']/cfg.meteo['prefix']YYYYMMDDHH``)
      to the int2lm input folder on scratch (``cfg.int2lm_input/meteo``).
    - For nested runs (meteorological files are COSMO output: ``cfg.meteo['prefix'] == 'lffd'``),
      also copy the ``*c.nc``-file with constant parameters.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.

    Raises
    ------
    RuntimeError
        If any subprocess returns a non-zero exit code during execution.
    """
    set_cfg_variables(cfg)
    tools.change_logfile(cfg.logfile)
    launch_time = cfg.init_time_logging("prepare_cosmo")

    logging.info('COSMO analysis data for IC/BC')

    dest_path = cfg.int2lm_input / 'meteo'
    tools.create_dir(dest_path, "meteo input")

    source_nameformat = cfg.meteo['nameformat']
    if cfg.meteo['prefix'] == 'lffd':
        # nested runs use cosmoart-output as meteo data
        # have to copy the *c.nc-file
        src_file = (cfg.meteo['dir'] /
                    cfg.startdate_sim.strftime(source_nameformat + 'c.nc'))

        tools.copy_file(src_file, dest_path, output_log=True)

        logging.info("Copied constant-param file from {} to {}".format(
            src_file, dest_path))

        # extend nameformat with ending to match cosmo-output
        source_nameformat += '.nc'

    if cfg.meteo['prefix'] == 'efsf':
        source_nameformat = cfg.meteo['prefix'] + '%y%m%d%H'

    num_steps = 0
    meteo_dir = cfg.meteo['dir']
    subdir = meteo_dir / cfg.startdate_sim.strftime('%y%m%d%H')
    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.meteo['inc']):
        dest_path = cfg.int2lm_input / 'meteo'
        src_file = meteo_dir / time.strftime(source_nameformat)

        if cfg.meteo['prefix'] == 'efsf':
            if time == cfg.startdate_sim:
                src_file = subdir / ('eas' + time.strftime('%Y%m%d%H'))
                if not src_file.exists() and cfg.meteo.get('dir_alt') \
                    is not None:
                    meteo_dir = cfg.meteo['dir_alt']
                    subdir = meteo_dir / cfg.startdate_sim.strftime('%y%m%d%H')
                    src_file = subdir / ('eas' + time.strftime('%Y%m%d%H'))
                dest_path = cfg.int2lm_input / 'meteo' / (cfg.meteo['prefix'] +
                                                          '00000000')
            else:
                td = time - cfg.startdate_sim - timedelta(hours=6 * num_steps)
                days = str(td.days).zfill(2)
                hours = str(td.seconds // 3600).zfill(2)
                td_total = time - cfg.startdate_sim
                days_total = str(td_total.days).zfill(2)
                hours_total = str(td_total.seconds // 3600).zfill(2)

                src_file = subdir / (cfg.meteo['prefix'] + days + hours +
                                     '0000')
                dest_path = cfg.int2lm_input / 'meteo' / (
                    cfg.meteo['prefix'] + days_total + hours_total + '0000')

                # Next time, change directory
                checkdir = meteo_dir / time.strftime('%y%m%d%H')
                if checkdir.is_dir():
                    num_steps += 1
                    subdir = checkdir
                elif cfg.meteo.get('dir_alt') is not None:
                    checkdir = cfg.meteo['dir_alt'] / time.strftime('%y%m%d%H')
                    if checkdir.is_dir():
                        num_steps += 1
                        subdir = checkdir
                        meteo_dir = cfg.meteo['dir_alt']
                        logging.info(
                            "Switching to other input directory from {} to {}".
                            format(cfg.meteo['dir'], cfg.meteo['dir_alt']))
        elif not src_file.exists():
            # special case for MeteoSwiss COSMO-7 data
            archive = Path('/store/mch/msopr/owm/COSMO-7')
            yy = time.strftime("%y")
            path = archive / 'ANA' + yy
            src_file = path / time.strftime(source_nameformat)

        # copy meteo file from project folder to
        tools.copy_file(src_file, dest_path, output_log=True)

        logging.info("Copied file from {} to {}".format(src_file, dest_path))

    # Other IC/BC data
    inv_to_process = []
    if cfg.workflow_name == 'cosmo-ghg':
        try:
            CAMS = dict(fullname="CAMS",
                        nickname="cams",
                        executable="cams4int2cosmo",
                        indir=cfg.cams['dir_orig'],
                        outdir=cfg.cams['dir_proc'],
                        param=[{
                            'inc': cfg.cams['inc'],
                            'suffix': cfg.cams['suffix']
                        }])
            inv_to_process.append(CAMS)
        except AttributeError:
            pass
        try:
            CT = dict(fullname="CarbonTracker",
                      nickname="ct",
                      executable="ctnoaa4int2cosmo",
                      indir=cfg.ct_dir_orig,
                      outdir=cfg.ct_dir_proc,
                      param=cfg.ct_parameters)
            inv_to_process.append(CT)
        except AttributeError:
            pass
    elif cfg.workflow_name == 'cosmo-art':
        try:
            MOZART = dict(fullname='MOZART',
                          nickname='mozart',
                          executable='mozart2int2lm',
                          indir=cfg.mozart_file_orig,
                          outdir=cfg.mozart_dir_proc,
                          param=[{
                              'inc': cfg.mozart_inc,
                              'suffix': cfg.mozart_prefix
                          }])
            inv_to_process.append(MOZART)
        except AttributeError:
            pass

    if cfg.workflow_name == 'cosmo-ghg' or cfg.workflow_name == 'cosmo-art':
        logging.info("Processing " +
                     ", ".join([i["fullname"]
                                for i in inv_to_process]) + " data")

        scratch_path = cfg.int2lm_input / 'icbc'
        tools.create_dir(scratch_path, "icbc input")

        for inv in inv_to_process:
            logging.info(inv["fullname"] + " files")
            tools.create_dir(inv["outdir"], "processed " + inv["fullname"])

            for p in inv["param"]:
                inc = p["inc"]
                for time in tools.iter_hours(cfg.startdate_sim,
                                             cfg.enddate_sim, inc):
                    logging.info(time)

                    filename = inv["outdir"] / (
                        p["suffix"] + "_" + time.strftime("%Y%m%d%H") + ".nc")
                    if not filename.exists():
                        logging.info(filename)
                        try:
                            to_call = getattr(tools, inv["executable"])
                            to_call.main(time, inv["indir"], inv["outdir"], p)
                        except:
                            logging.error("Preprocessing " + inv["fullname"] +
                                          " data failed")
                            raise

                    # copy to (temporary) run input directory
                    tools.copy_file(filename, scratch_path, output_log=True)

                    logging.info("OK")

    cfg.finish_time_logging("prepare_cosmo", launch_time)
