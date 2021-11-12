#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess
import logging
import os
import shutil
import time
import sys

from datetime import datetime, timedelta
from enum import Enum, auto

from . import cams4int2cosmo
from . import ctnoaa4int2cosmo
from . import string2char
from . import vprmsplit
from . import write_int2lm_input_art
#from . import write_cosmo_input_ghg
from . import mozart2int2lm
from .check_target import check_target
from . import comp_nc
from . import helper


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


def prepare_message(logfile_path):
    """Shortens the logfile to be sent via mail if it is too long.

    Parameters
    ----------
    logfile_path : str
        Path of logfile
    """

    with open(logfile_path) as logfile:
        message = logfile.read()
    # Shorten big logfiles
    if len(message) > 4096:
        message = message[:2048] + \
                  "\n\n--------------------------------------------------\n" + \
                  "### Some lines are skipped here. Original logfile:\n" + \
                  logfile_path + \
                  "\n--------------------------------------------------\n\n" + \
                  message[-2048:]

    return message


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
    p = subprocess.Popen(
        ['mail', '-s', '"%s"' % subject, address], stdin=subprocess.PIPE)
    p.stdin.write(message.encode('utf-8'))
    p.stdin.close()


def change_logfile(filename):
    """Change the path of the logfile used by the logging module.

    Parameters
    ----------
    filename : str
        Path to the new logfile
    """

    fileh = logging.FileHandler(filename, 'a', delay=True)
    # log_format = logging.Formatter('%(levelname)s:%(message)s')
    # fileh.setFormatter(log_format)

    log = logging.getLogger()  # root logger
    if len(log.handlers) > 0:
        log.handlers = [fileh]  # set the new handler
    else:
        logging.basicConfig(filename=filename, level=logging.INFO)


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
        logging.error("Creating {} directory at path {} failed with {}".format(
            readable_name, path,
            type(e).__name__))
        raise


def copy_file(source_path, dest_path, output_log=False):
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
                      "target-directory {} doesn't exist.".format(
                          source_path, dest_path))
        raise
    except PermissionError:
        logging.error("Copying file from {} to {} failed due to"
                      "a permission error.".format(source_path, dest_path))
        raise
    except (OSError, Exception) as e:
        logging.error("Copying {} to {} failed with {}".format(
            source_path, dest_path,
            type(e).__name__))
        raise
    logging.info("Copied {} to {}".format(source_path, dest_path))


def rename_file(source_path, dest_path, output_log=False):
    """Copy a file from source_path to dest_path

    Use os.rename to rename the file.
    dest_path can be either a directory or a filepath.
    If it is a directory, the name of the file will be
    kept, if it is a filepath the file will be renamed.
    File permissions will be kept as well.

    Provides error description to the logfiles

    Parameters
    ----------
    source_path : str
        Path to the file to be renamed
    dest_path : str
        Path to the destination directory or destination file
    """
    try:
        os.rename(source_path, dest_path)
    except FileNotFoundError:
        logging.error("Source-file not found at {} OR "
                      "target-directory {} doesn't exist.".format(
                          source_path, dest_path))
        raise
    except PermissionError:
        logging.error("Copying file from {} to {} failed due to"
                      "a permission error.".format(source_path, dest_path))
        raise
    except (OSError, Exception) as e:
        logging.error("Copying {} to {} failed with {}".format(
            source_path, dest_path,
            type(e).__name__))
        raise
    logging.info("Renamed {} to {}".format(source_path, dest_path))


def remove_file(dest_path, output_log=False):
    """Delete a file at dest_path

    Use os.remove to delete the file.
    dest_path has to be a filepath.

    Provides error description to the logfiles

    Parameters
    ----------
    dest_path : str
        Path to the destination file
    """
    try:
        os.remove(dest_path)
    except FileNotFoundError:
        logging.error("Target-file not found at {}.".format(dest_path))
        raise
    except PermissionError:
        logging.error("Removing file {} failed due to"
                      "a permission error.".format(dest_path))
        raise
    except (OSError, Exception) as e:
        logging.error("Removing {} failed with {}".format(
            dest_path,
            type(e).__name__))
        raise
    logging.info("Removed {}".format(dest_path))


class Target(Enum):
    COSMO = auto()
    COSMOART = auto()
    COSMOGHG = auto()
    ICON = auto()
    ICONART = auto()
    ICONARTOEM = auto()


class Subtarget(Enum):
    NONE = auto()
    SPINUP = auto()


str_to_enum = {
    'cosmo': Target.COSMO,
    'cosmo-art': Target.COSMOART,
    'cosmo-ghg': Target.COSMOGHG,
    'icon': Target.ICON,
    'icon-art': Target.ICONART,
    'icon-art-oem': Target.ICONARTOEM,
    'none': Subtarget.NONE,
    'spinup': Subtarget.SPINUP,
}


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
                           "is {}, but the job only applies to {}.".format(
                               cfg.target.name, target.name))


def levenshtein(s1, s2):
    """Return the levenshtein distance ("edit distance") between s1 and s2.

    From https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Python

    Used to suggest the closest existing casename when giving a non-existent one.

    >>> levenshtein('car', 'cab')
    1
    >>> levenshtein('Hello how', 'are you')
    7
    """
    if len(s1) < len(s2):
        return levenshtein(s2, s1)

    # len(s1) >= len(s2)
    if len(s2) == 0:
        return len(s1)

    previous_row = range(len(s2) + 1)
    for i, c1 in enumerate(s1):
        current_row = [i + 1]
        for j, c2 in enumerate(s2):
            # j+1 instead of j since previous_row and current_row are one
            # character longer than s2
            insertions = previous_row[j + 1] + 1
            deletions = current_row[j] + 1
            substitutions = previous_row[j] + (c1 != c2)
            current_row.append(min(insertions, deletions, substitutions))
        previous_row = current_row

    return previous_row[-1]


def grep(string, filename):
    """Mimics the "grep" function.

    Parameters
    ----------
    string : string to be searched for

    filename: file name in which string is searched
    """

    # List of lines where string is found
    list_line = []
    list_iline = []
    lo_success = False

    for iline, line in enumerate(open(filename)):
        if string in line:
            list_line.append(line)
            list_iline.append(iline)
            lo_success = True

    return {"success": lo_success, "iline": list_iline, "line": list_line}


def check_job_completion(log_finished_dir, job, waittime=3000):
    """Check that a certain job is done, otherwise waits 300 seconds.

    Parameters
    ----------
    cfg : config-object

    log_finished_dir : directory for logfiles of finished jobs

    job: string of job name, e.g. "meteo"

    waittime : time to wait (factor of .1 second)
               Defaults to 3000 (300 seconds)
    """
    logfile = os.path.join(log_finished_dir, job)
    while True:
        if not os.path.exists(logfile):
            print("Waiting for the %s job to finish first" % (job))
            sys.stdout.flush()
            for _ in range(waittime):
                time.sleep(0.1)
        else:
            break
