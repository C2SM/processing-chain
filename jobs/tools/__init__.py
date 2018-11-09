#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess
import logging
import os
import shutil

from datetime import datetime, timedelta
from enum import Enum, auto

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
    If the timeperiod is divisible by the step, the last timepoint will be
    exactly ``starttime + hstop``. If not, the last timepoint will be before
    that.

    >>> import datetime
    >>> date = datetime.datetime.strptime("20150101", "%Y%m%d")
    >>> [t.hour for t in iter_hours(date, 10, 14, 2)]
    [10, 12, 14]
    >>> [t.hour for t in iter_hours(date, 9, 16, 3)]
    [9, 12, 15]
    """
    assert hstop > hstart, "Start has to be before stop (hstop > hstart)"
    current = starttime + timedelta(hours=hstart)
    stoptime = starttime + timedelta(hours=hstop)

    while current <= stoptime:
        yield current
        current += timedelta(hours=step)


def send_mail(address, subject, message=''):
    """Send an email to adress.

    Start a subprocess that sends an email using the linux ``mail```-
    command.

    Parameters
    ----------
    address : str
        Adress to which the email is sent
    subject : str
        Subject of the message
    message : str, optional
        Body of the message, default is empty
    """
    p = subprocess.Popen(['mail', '-s', '"%s"' % subject, address], stdin=subprocess.PIPE)
    p.stdin.write(message.encode('utf-8'))
    p.stdin.close()


def change_logfile(filename):
    """Change the path of the logfile used by the logging module.

    Parameters
    ----------
    filename : str
        Path to the new logfile
    """
    fileh = logging.FileHandler(filename, 'a')
    # log_format = logging.Formatter('%(levelname)s:%(message)s')
    # fileh.setFormatter(log_format)

    log = logging.getLogger()  # root logger
    if len(log.handlers)>0:
        log.handlers = [fileh]      # set the new handler
    else:
        logging.basicConfig(filename=filename,level=logging.INFO)


def create_dir(path, readable_name):
    """Create a directory at path, log failure using readable_name.
    
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


def copy_file(source_path, dest_path):
    """Copy a file from source_path to dest_path

    Use shutil.copy to copy the file.
    dest_path can be either a directory or a filepath.
    If it is a directory, the name of the file will be
    kept, if it is a filepath the file will be renamed.
    File permissions will be copied as well.

    Provides error description to the logfiles

    Parameters
    ----------
    source_path : str
        Path to the file to be copied
    dest_path : str
        Path to the destination directory or destination file
    """
    try:
        shutil.copy(source_path, dest_path)
    except FileNotFoundError:
        logging.error("Source-file not found at {} OR "
                      "target-directory {} doesn't exist."
                      .format(source_path, dest_path))
        raise
    except PermissionError:
        logging.error("Copying file from {} to {} failed due to"
                      "a permission error.".format(source_path, dest_path))
    except (OSError, Exception) as e:
        logging.error("Copying {} to {} failed with {}". format(
                      source_path, dest_path, type(e).__name__))
        raise


class Target(Enum):
    COSMO = auto()
    COSMOART = auto()

str_to_enum = {'cosmo': Target.COSMO, 'cosmoart': Target.COSMOART}


def check_target(cfg, target=Target.COSMO):
    """Check that the target specified in cfg matched the prescribed target.

    Check that cfg.target == target. If not, raises a RuntimeError.

    Parameters
    ----------
    cfg : config-object

    target : Target enum
        Prescribed target
    """
    if not cfg.target is target:
        raise RuntimeError("The target specified in the configuration file "
                           "is {}, but the job only applies to {}."
                           .format(cfg.target.name, target.name))
