#!/usr/bin/env python
# -*- coding: utf-8 -*-


from datetime import datetime, timedelta
import subprocess
import logging
import os
from . import cams4int2cosmo
from . import ctnoaa4int2cosmo
from . import string2char
from . import vprmsplit
from . import write_int2lm_input_art
from . import write_cosmo_input_bgc
from . import mozart2int2lm
from .check_target import check_target
from . import comp_nc

def iter_hours(starttime, hstart, hstop, step=1):
    """Return a generator that yields datetime-objects from 
    ``starttime + hstart`` up to (and possibly including) ``starttime + hstop`` 
    in ``step``-increments.
    
    If no ```step`` is given, the stepsize is 1 hour

    Parameters
    ----------
    starttime : datetime-object
        The start-date if the iteration
    hstart : int
        Offset (in hours) from the starttime where the iteration starts
    hstop : int
        Offset (in hours) from the starttime where the iteration stops
    step : int, optional
        Stepsize, defaults to 1
        
    Yields
    ------
    datetime-object
        The next timepoint in the iteration
        
    Examples
    --------
    If ``(hstop - hstart) / step`` is not an integer, the last timepoint will
    be before ``starttime + hstop``.
    
    >>> import datetime.datetime.strptime
    >>> [t.hour for t in iter_hours(strptime("20150101", "%Y%m%d"), 10, 15, 2)]
    [10, 12, 14]
    """
    assert hstop > hstart, "Start has to be before stop (hstop > hstart)"
    current = starttime + timedelta(hours=hstart)
    stoptime = starttime + timedelta(hours=hstop)

    while current <= stoptime:
        yield current
        current += timedelta(hours=step)

def iter_times(start, end, step):
    """Is this needed?
    """
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
        log.handlers = [fileh]      # set the new handler
    else:
        logging.basicConfig(filename=filename,level=logging.INFO)


def create_dir(path, readable_name):
    """Create a directory at path, log failure using readable_name
    
    Use ``os.makedirs(path, exist_ok=True)`` to create all necessary 
    directories for ``path`` to point to a valid directory. Do nothing if the
    directory at ``path`` already exists.
    
    If the directory can not be created, log an error-message logged and raise
    the exception.
    
    Parameters
    ----------
    path : str
        Path of the directory to be created
    readable_name : str
        Name of the directory used in the logging messsage
    """
    try:
        os.makedirs(path, exist_ok=True)
    except (OSError, Exception) as e:
        logging.error(
            "Creating {} directory at path {} failed with {}".format(
                readable_name, path, type(e).__name__))
        raise
   
