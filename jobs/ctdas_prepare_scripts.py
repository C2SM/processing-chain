#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
from re import A
from . import tools
from datetime import timedelta
import shutil


def main(starttime, hstart, hstop, cfg):

    # Set up SBATCH extraction template
    tools.create_dir(os.path.join(cfg.icon_work, 'templates'),
                     "ctdas_icon_templates")
    shutil.copyfile(
        cfg.ctdas_sbatch_extract_template,
        os.path.join(cfg.icon_work, 'templates', 'sbatch_extract_template'))
    cfg.ctdas_sbatch_extract_template_scratch = os.path.join(
        cfg.icon_work, 'templates', 'sbatch_extract_template')

    # Link the first simulation
    restart_first_sim_init = os.path.join(
        cfg.ctdas_first_restart_init,
        'ICON-ART-OEM-INIT_' + '%sT00:00:00.000.nc' %
        ((starttime + timedelta(hours=24)).strftime('%Y-%m-%d')))
    link_restart_dir = os.path.join(
        cfg.icon_base, 'output_%s_opt' %
        ((starttime -
          timedelta(hours=cfg.ctdas_cycle * 24)).strftime('%Y%m%d%H')))
    restart_file = os.path.join(
        link_restart_dir, 'ICON-ART-OEM-INIT_%sT00:00:00.000.nc' %
        ((starttime + timedelta(hours=24)).strftime('%Y-%m-%d')))
    tools.create_dir(link_restart_dir,
                     "restart ini dir for the first CTDAS simulation")
    os.system('ln -sf ' + restart_first_sim_init + ' ' + restart_file)

    # Write script files for each ctdas lag
    for time in tools.iter_hours(starttime, hstart, hstop,
                                 cfg.ctdas_cycle * 24):
        # Set the starting date of the current cycle (filenames are based on this)
        current_cycle_filename = time.strftime('%Y%m%d%H')

        # Go back [ctdas-cycle * 24] hours to find the ICON restart file
        restart_t = (
            time - timedelta(hours=cfg.ctdas_cycle * 24)).strftime('%Y%m%d%H')

        # Allow for spinup
        spinup_time = time + timedelta(seconds=cfg.ctdas_restart_init_time)

        # Limit the simulation end-time if needed
        a = spinup_time + timedelta(days=cfg.ctdas_cycle)
        if a < starttime + timedelta(hours=hstop - hstart):
            end_time = a
        else:
            end_time = starttime + timedelta(hours=hstop - hstart)

        # Name of the ICBC file
        ICBC_filename = os.path.join(
            cfg.icon_input_icbc,
            time.strftime(cfg.meteo_nameformat) + '.nc')

        # File name for the extraction output
        extraction_output_file_name = os.path.join(
            cfg.icon_base, 'extracted',
            'output_%s_' % (current_cycle_filename))

        # Lambdas for emissions & background
        lambda_fs = os.path.join(cfg.icon_base, 'input', 'oae',
                                 'lambda_%s_' % (current_cycle_filename))
        bg_lambda_fs = os.path.join(cfg.icon_base, 'input', 'oae',
                                    'bg_lambda_%s_' % (current_cycle_filename))

        # Restart folders & filenames
        restart_folder_opt = os.path.join(cfg.icon_base,
                                          'output_%s_opt' % (restart_t))
        restart_folder_prior = os.path.join(
            cfg.icon_base, 'output_%s_priorcycle1' % (restart_t))
        restart_file = 'ICON-ART-OEM-INIT_%sT00:00:00.000.nc' % (
            (spinup_time).strftime('%Y-%m-%d'))

        # Create extraction & runscripts
        with open(cfg.ctdas_extract_template) as input_file:
            write_extraction = input_file.read()
        extract_file = os.path.join(cfg.icon_work,
                                    'extract_' + current_cycle_filename)

        with open(cfg.ctdas_ICON_template) as input_file:
            write_runscript = input_file.read()
        runscript_file = os.path.join(cfg.icon_work,
                                      'runscript_' + current_cycle_filename)

        for case in ['prior', 'priorcycle1', 'opt']:
            # Set the case
            setattr(cfg, 'suffix', case)

            # Write the extraction script
            with open(extract_file + cfg.suffix, "w") as outf:
                outf.write(
                    write_extraction.format(
                        cfg=cfg,
                        icon_path=cfg.icon_output + '_' +
                        time.strftime('%Y%m%d%H') + '_' + cfg.suffix,
                        fname_base='ICON-ART-UNSTR',
                        stationdir=cfg.ctdas_datadir,
                        nneighb=5,
                        start=spinup_time,
                        end=end_time,
                        extracted_file=extraction_output_file_name +
                        cfg.suffix))

            # Write the runscript
            ICON_output_folder = cfg.icon_output + '_' + time.strftime(
                '%Y%m%d%H') + '_' + cfg.suffix
            restart_folder = restart_folder_opt if (
                case != 'prior') else restart_folder_prior
            with open(runscript_file + cfg.suffix, "w") as outf:
                outf.write(
                    write_runscript.format(
                        cfg=cfg,
                        ini_restart_string=time.strftime('%Y-%m-%dT%H:%M:%SZ'),
                        ini_restart_end_string=end_time.strftime(
                            '%Y-%m-%dT%H:%M:%SZ'),
                        inifile=ICBC_filename,
                        lambda_f=lambda_fs + cfg.suffix + '.nc',
                        bg_lambda=bg_lambda_fs + cfg.suffix + '.nc',
                        emissionsgrid=cfg.ctdas_oae_grid.format(
                            time.strftime(cfg.ctdas_oae_grid_fname))
                        if "{}" in cfg.ctdas_oae_grid else cfg.ctdas_oae_grid,
                        restart_file=os.path.join(restart_folder,
                                                  restart_file),
                        output_directory=ICON_output_folder))
            tools.create_dir(ICON_output_folder,
                             "creating ICON output folder...")
