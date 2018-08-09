#!/usr/bin/env python
# -*- coding: utf-8 -*-


from datetime import datetime, timedelta
import subprocess
import logging
from . import cams4int2cosmo
from . import ctnoaa4int2cosmo
from . import string2char
from . import vprmsplit
from . import write_int2lm_input_art
from . import write_cosmo_input_bgc

def iter_hours(starttime, hstart, hstop, step=1):

    current = starttime + timedelta(hours=hstart)
    stoptime = starttime + timedelta(hours=hstop)

    while current <= stoptime:
        yield current
        current += timedelta(hours=step)

def iter_times(start, end, step):
    current = start
    while current < end:
        yield current
        current += step

def send_mail(address, subject, message=''):
    p = subprocess.Popen(['mail', '-s', '"%s"' % subject, address], stdin=subprocess.PIPE)
    p.stdin.write(message.encode('utf-8'))
    p.stdin.close()

def change_logfile(filename):
    fileh = logging.FileHandler(filename, 'a')
    # log_format = logging.Formatter('%(levelname)s:%(message)s')
    # fileh.setFormatter(log_format)

    log = logging.getLogger()  # root logger
    if len(log.handlers)>0:
        log.addHandler = fileh      # set the new handler
    else:
        logging.basicConfig(filename=filename,level=logging.INFO)

    

