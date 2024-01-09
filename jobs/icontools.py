#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
from . import tools, prepare_data


def main(cfg):
    """
    - Submit the runscript for the DWD ICON tools to remap the meteorological files.
    - All runscripts specified in ``cfg.icontools_runjobs`` are submitted.
    - The meteorological files are read from the original input directory
      (``cfg.input_root_meteo``), and the remapped meteorological files are saved
      in the input folder on scratch (``cfg.icon_input/icbc``).
    """
    prepare_data.set_cfg_variables(cfg)
    launch_time = cfg.init_time_logging("icontools")
    logfile = cfg.log_working_dir / "icontools"
    logfile_finish = cfg.log_finished_dir / "icontools"

    #-----------------------------------------------------
    # Create LBC datafile lists (each at 00 UTC and others)
    #-----------------------------------------------------
    datafile_list = []
    datafile_list_rest = []
    datafile_list_chem = []
    for time in tools.iter_hours(cfg.startdate_sim, cfg.enddate_sim,
                                 cfg.meteo['inc']):
        meteo_file = cfg.icon_input_icbc / (
            cfg.meteo['prefix'] + time.strftime(cfg.meteo['nameformat']))
        if cfg.workflow_name == 'icon-art' or cfg.workflow_name == 'icon-art-oem':
            chem_file = cfg.icon_input_icbc / (
                cfg.chem['prefix'] + time.strftime(cfg.chem_nameformat))
            datafile_list_chem.append(str(chem_file) + cfg.chem['suffix'])
        if str(meteo_file).endswith('00'):
            datafile_list.append(str(meteo_file) + cfg.meteo['suffix'])
        else:
            datafile_list_rest.append(str(meteo_file) + cfg.meteo['suffix'])
    datafile_list = ' '.join([str(v) for v in datafile_list])
    datafile_list_rest = ' '.join([str(v) for v in datafile_list_rest])
    datafile_list_chem = ' '.join([str(v) for v in datafile_list_chem])

    #-----------------------------------------------------
    # Write and submit runscripts
    #-----------------------------------------------------
    dep_id = None
    for runscript in cfg.icontools_runjobs:
        with (cfg.case_path / runscript).open() as input_file:
            to_write = input_file.read()
        runscript_path = cfg.icon_work / f"{runscript}.job"
        with runscript_path.open("w") as outf:
            outf.write(
                to_write.format(cfg=cfg,
                                meteo=cfg.meteo,
                                logfile=logfile,
                                logfile_finish=logfile_finish,
                                datafile_list=datafile_list,
                                datafile_list_rest=datafile_list_rest,
                                datafile_list_chem=datafile_list_chem))

        # Submitting icontools runscripts sequentially
        logging.info(f" Starting icontools runscript {runscript}.")
        dep_id = cfg.submit('icontools', runscript_path, add_dep=dep_id)

    cfg.finish_time_logging("icontools", launch_time)
