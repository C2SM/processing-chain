#!/usr/bin/env python
# -*- coding: utf-8 -*-

from datetime import datetime, timedelta

import importlib
import logging
import warnings
import os
import subprocess
import sys
import time
import shutil

import jobs
from jobs import tools


# try to load config file
try:
    fn = os.path.join('cases',sys.argv[1],'config')
    sys.path.append(os.path.dirname(fn))
    cfg = importlib.import_module(os.path.basename(fn))
except IndexError:
    print('ERROR: no config file provided!')
    sys.exit(1)
except ImportError:
    print('ERROR: failed to import config module "%s"!' % fn)
    sys.exit(1)


def run_chain(work_root, start_time, hstart=0.0, hstop=24.0, step=24.0,
              job_names=None):
    """\
    Run complete chain ignoring already finished jobs.
    """
    # ini date and forecast time (ignore meteo times)
    inidate = int((start_time - datetime(1970,1,1)).total_seconds())
    inidate_yyyymmddhh = start_time.strftime('%Y%m%d%H')
    inidate_int2lm_yyyymmddhh = (start_time + timedelta(hours=hstart)
                                ).strftime('%Y%m%d%H')
    forecasttime = '%d' % (hstop - hstart)

    setattr(cfg, 'inidate', inidate)
    setattr(cfg, 'inidate_yyyymmddhh',inidate_yyyymmddhh)
    setattr(cfg, 'forecasttime', forecasttime)
    setattr(cfg, 'hstart', hstart)
    setattr(cfg, 'hstop', hstop)

    # int2lm processing always starts at hstart=0 and we modify inidate instead
    setattr(cfg, 'inidate_int2lm_yyyymmddhh', inidate_int2lm_yyyymmddhh)
    setattr(cfg, 'hstart_int2lm', '0')
    setattr(cfg, 'hstop_int2lm', forecasttime)

    # chain 
    job_id = '%s_%d_%d' % (inidate_yyyymmddhh, hstart, hstop)
    chain_root = os.path.join(work_root, cfg.casename, job_id)
    setattr(cfg, 'chain_root', chain_root)

    # INT2LM
    setattr(cfg, 'int2lm_base', os.path.join(chain_root, 'int2lm'))
    setattr(cfg, 'int2lm_input', os.path.join(chain_root, 'int2lm', 'input'))
    setattr(cfg, 'int2lm_work', os.path.join(chain_root, 'int2lm', 'run'))
    setattr(cfg, 'int2lm_output', os.path.join(chain_root, 'int2lm', 'output'))

    # COSMO
    setattr(cfg, 'cosmo_base', os.path.join(chain_root, 'cosmo'))
    setattr(cfg, 'cosmo_work', os.path.join(chain_root, 'cosmo', 'run'))
    setattr(cfg, 'cosmo_output', os.path.join(chain_root, 'cosmo', 'output'))

    job_id_last_run = '%s_%d_%d' % (inidate_yyyymmddhh, 
                                    hstart - step, hstop - step)
    chain_root_last_run = os.path.join(work_root, cfg.casename,
                                       job_id_last_run)
    setattr(cfg, 'cosmo_restart_in', os.path.join(chain_root_last_run,
                                                 'cosmo', 'restart')
           )
    setattr(cfg, 'cosmo_restart_out', os.path.join(chain_root, 
                                                  'cosmo', 'restart')
           )

    # logging
    log_working_dir = os.path.join(chain_root, 'checkpoints', 'working')
    log_finished_dir = os.path.join(chain_root, 'checkpoints', 'finished')
    setattr(cfg, 'log_working_dir', log_working_dir)
    setattr(cfg, 'log_finished_dir', log_finished_dir)

    # create working dirs
    if not os.path.exists(chain_root):
        os.makedirs(chain_root)
        os.makedirs(log_working_dir)
        os.makedirs(log_finished_dir)

    # run jobs (if required)
    if job_names == [] or job_names is None:
        job_names = [
            'meteo', 'icbc', 'emissions', 'biofluxes',
            'int2lm', 'post_int2lm',
            'cosmo', 'post_cosmo'
        ]

    for job in job_names:

        # mapping of scripts in jobs with their arguments

        # if job == 'meteo':
        #     job.meteo.main(start_time, hstart, hstop, cfg)
        #     continue

        skip = False

        # if exists job is currently worked on or has been finished
        if os.path.exists( os.path.join(log_working_dir, job) ):

            while True:
                if os.path.exists( os.path.join(log_finished_dir, job) ):
                    print('Skip "%s" for chain "%s"' % (job, job_id))
                    skip = True
                    break
                else:
                    print('Wait for "%s" of chain "%s"' % (job, job_id))
                    sys.stdout.flush()
                    for _ in range(3000):
                        time.sleep(0.1)

        if not skip:
            print('Process "%s" for chain "%s"' % (job, job_id))
            sys.stdout.flush()
            
            try:
                # Change the log file
                logfile=os.path.join(cfg.log_working_dir,job)
                logfile_finish=os.path.join(cfg.log_finished_dir,job)
                tools.change_logfile(logfile)

                # Launch the job
                to_call = getattr(jobs,job)                
                to_call.main(start_time,hstart,hstop,cfg)
                
                shutil.copy(logfile, logfile_finish)

                exitcode=0
            except:
                subject = "ERROR or TIMEOUT in job '%s' for chain '%s'" % (job,
                          job_id)
                logging.exception(subject)
                with open(os.path.join(log_working_dir, job)) as logfile:
                    message = logfile.read()
                tools.send_mail(cfg.mail_address, subject, message)
                raise RuntimeError(subject)
                
            # except AttributeError:
            #     print(job+".py not found so running the bash script instead")
            #     exitcode = call_bash_function(
            #         os.path.join(cfg.chain_src_dir, 'jobs', '%s.bash' % job),
            #         job
            #     )
                
            if exitcode != 0 or not os.path.exists(os.path.join(log_finished_dir, job)):
                subject = "ERROR or TIMEOUT in job '%s' for chain '%s'" % (job,
                          job_id)
                with open(os.path.join(log_working_dir, job)) as logfile:
                    message = logfile.read()
                tools.send_mail(cfg.mail_address, subject, message)
                raise RuntimeError(subject)


def restart_runs(start, work_root, hstart=0, hstop=239, job_names=None):

    end = start + timedelta(hours=hstop) 
    step = hstop # in hours (has to be multiple of 3)

    # run restarts
    for time in tools.iter_times(start + timedelta(hours=hstart), end,
                                 timedelta(hours=cfg.restart_step)
                                ):
        print(time)
        if cfg.restart_step > hstop:
            step = hstop
        else:
            step = cfg.restart_step
        hstart = (time - start).total_seconds() / 3600.0
        hstop = hstart + step

        try:
          run_chain(work_root, start, hstart, hstop, step,
                    job_names=job_names)
        except RuntimeError:
            sys.exit(1)


if __name__ == '__main__':
    # TODO: use argparse
    start_time = datetime.strptime(sys.argv[2], '%Y-%m-%d')
    hstart = int(sys.argv[3])
    hstop = int(sys.argv[4])
    job_names = sys.argv[5:]

    restart_runs(start_time, cfg.work_root, hstart=hstart, hstop=hstop,
                 job_names=job_names)
    
    print('>>> finished chain for good or bad! <<<')
