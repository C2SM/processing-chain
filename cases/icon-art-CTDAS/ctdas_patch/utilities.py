#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""

Created on Wed Sep 18 16:03:02 2019

@author: friedemann
"""

import os
import glob
import logging
import subprocess
import tempfile
import copy
import netCDF4 as nc
import numpy as np


class utilities(object):
    """
    Collection of utilities for wrfchem observation operator
    that do not depend on other CTDAS modules
    """

    def __init__(self):
        pass

    @staticmethod
    def get_slicing_ids(N, nproc, nprocs):
        """
        Purpose
        -------
        For parallel processing, figure out which samples to process
        by this process.

        Parameters
        ----------
        N : int
            Length to slice
        nproc : int
            id of this process (0... nprocs-1)
        nprocs : int
            Number of processes that work on the task.

        Output
        ------
        Slicing indices id0, id1
        Usage
        -----
            ..code-block:: python

            id0, id1 = get_slicing_ids(N, nproc, nprocs)
            field[id0:id1, ...]
        """

        f0 = float(nproc) / float(nprocs)
        id0 = int(np.floor(f0 * N))

        f1 = float(nproc + 1) / float(nprocs)
        id1 = int(np.floor(f1 * N))

        if id0 == id1:
            raise ValueError("id0==id1. Probably too many processes.")
        return id0, id1

    @classmethod
    def cat_ncfiles(cls,
                    path,
                    in_arg,
                    cat_dim,
                    out_file,
                    in_pattern=False,
                    rm_original=True):
        """
        Combine output of all processes into 1 file
        If in_pattern, a pattern is provided instead of a file list.
        This has the advantage that it can be interpreted by the shell,
        because there are problems with long argument lists.

        This calls ncrcat from the nco library. If nco is not available,
        rewrite this function. Note: I first tried to do this with
        "cdo cat", but it messed up sounding_id
        (see https://code.mpimet.mpg.de/boards/1/topics/908)
        """

        # To preserve dimension names, we start from one of the existing
        # slice files instead of a new file.

        # Do this in path to avoid long command line arguments and history
        # entries in outfile.
        cwd = os.getcwd()
        os.chdir(path)

        if in_pattern:
            if not isinstance(in_arg, str):
                raise TypeError(
                    "in_arg must be a string if in_pattern is True.")
            file_pattern = in_arg
            in_files = glob.glob(file_pattern)
        else:
            if isinstance(in_arg, list):
                raise TypeError(
                    "in_arg must be a list if in_pattern is False.")
            in_files = in_arg

        if len(in_files) == 0:
            logging.error("Nothing to do.")
            # Change back to previous directory
            os.chdir(cwd)
            return

        # Sorting is important!
        in_files.sort()

        # ncrcat needs total number of soundings, count
        Nobs = 0
        for f in in_files:
            ncf = nc.Dataset(f, "r")
            Nobs += len(ncf.dimensions[cat_dim])
            ncf.close()

        # Cat files
        cmd_ = "ncrcat -h -O -d " + cat_dim + ",0,%d" % (Nobs - 1)
        if in_pattern:
            cmd = cmd_ + " " + file_pattern + " " + out_file
            # If PIPE is used here, it gets clogged, and the process
            # stops without error message (see also
            # https://thraxil.org/users/anders/posts/2008/03/13/Subprocess-Hanging-PIPE-is-your-enemy/)
            # Hence, piping the output to a temporary file.
            proc = subprocess.Popen(cmd,
                                    shell=True,
                                    stdout=tempfile.TemporaryFile(),
                                    stderr=tempfile.TemporaryFile())
        else:
            cmdsplt = cmd_.split() + in_files + [out_file]
            proc = subprocess.Popen(cmdsplt,
                                    stdout=tempfile.TemporaryFile(),
                                    stderr=tempfile.TemporaryFile())
            cmd = " ".join(cmdsplt)

        proc.wait()

        # This is probably useless since the output is piped to a
        # tempfile.
        retcode = cls.check_out_err(proc)

        if retcode != 0:
            msg = "Something went wrong in the sampling. Command: " + cmd
            logging.error(msg)
            raise OSError(msg)

        # Delete slice files
        if rm_original:
            logging.info("Deleting slice files.")
            for f in in_files:
                os.remove(f)

        logging.info("Sampled WRF output written to file.")

        # Change back to previous directory
        os.chdir(cwd)

    @staticmethod
    def check_out_err(process):
        """Displays stdout and stderr, returns returncode of the
        process.
        """

        # Get process messages
        out, err = process.communicate()

        # Print output
        def to_str(str_or_bytestr):
            """If argument is of type str, return argument. If
            argument is of type bytes, return decoded str"""
            if isinstance(str_or_bytestr, str):
                return str_or_bytestr
            elif isinstance(str_or_bytestr, bytes):
                return str(str_or_bytestr, 'utf-8')
            else:
                msg = "str_or_bytestr is " + str(type(str_or_bytestr)) + \
                      ", should be str or bytestr."
                raise TypeError(msg)

        logging.debug("Subprocess output:")
        if out is None:
            logging.debug("No output.")
        elif isinstance(out, list):
            for line in out:
                logging.debug(to_str(line.rstrip()))
        else:
            logging.debug(to_str(out.rstrip()))

        # Handle errors
        if process.returncode != 0:
            logging.error("subprocess error")
            logging.error("Returncode: %s", str(process.returncode))
            logging.error("Message, if any:")
            if not err is None:
                for line in err:
                    logging.error(line.rstrip())

        return process.returncode

    @classmethod
    def get_index_groups(cls, *args):
        """
        Input:
            numpy arrays with 1 dimension or lists, all same length
        Output:
            Dictionary of lists of indices that have the same
            combination of input values.
        """

        try:
            # If pandas is available, it makes a pandas DataFrame and
            # uses its groupby-function.
            import pandas as pd

            args_array = np.array(args).transpose()
            df = pd.DataFrame(args_array)
            groups = df.groupby(list(range(len(args)))).indices

        except ImportError:
            # If pandas is not available, use an own implementation of groupby.
            # Recursive implementation. It's fast.
            args_array = np.array(args).transpose()
            groups = cls._group(args_array)

        return groups

    @classmethod
    def _group(cls, a):
        """
        Reimplementation of pandas.DataFrame.groupby.indices because
        py 2.7 on cartesius isn't compatible with pandas.
        Unlike the pandas function, this always uses all columns of the
        input array.
    
        Parameters
        ----------
        a : numpy.ndarray (2D)
            Array of indices. Each row is a combination of indices.
    
        Returns
        -------
        groups : dict
            The keys are the unique combinations of indices (rows of a),
            the values are the indices of the rows of a equal the key.
        """

        # This is a recursive function: It makes groups according to the
        # first columnm, then calls itself with the remaining columns.
        # Some index juggling.

        # Group according to first column
        UI = list(set(a[:, 0]))
        groups0 = dict()
        for ui in UI:
            # Key must be a tuple
            groups0[(ui, )] = [i for i, x in enumerate(a[:, 0]) if x == ui]

        if a.shape[1] == 1:
            # If the array only has one column, we're done
            return groups0
        else:
            # If the array has more than one column, we group those.
            groups = dict()
            for ui in UI:
                # Group according to the remaining columns
                subgroups_ui = cls._group(a[groups0[(ui, )], 1:])
                # Now the index juggling: Add the keys together and
                # locate values in the original array.
                for key in list(subgroups_ui.keys()):
                    # Get indices of bigger array
                    subgroups_ui[key] = [
                        groups0[(ui, )][n] for n in subgroups_ui[key]
                    ]
                    # Add the keys together
                    groups[(ui, ) + key] = subgroups_ui[key]

            return groups

    @staticmethod
    def apply_by_group(func,
                       array,
                       groups,
                       grouped_args=None,
                       *args,
                       **kwargs):
        """
        Apply function 'func' to a numpy array by groups of indices.
        'groups' can be a list of lists or a dictionary with lists as
        values.
    
        If 'array' has more than 1 dimension, the indices in 'groups'
        are for the first axis.
    
        If 'grouped_args' is not None, its members are added to
        'kwargs' after slicing.
    
        *args and **kwargs are passed through to 'func'.
    
        Example:
            apply_by_group(np.mean, np.array([0., 1., 2.]), [[0, 1], [2]])
        Output:
            array([0.5, 2. ])
        """

        shape_in = array.shape
        shape_out = list(shape_in)
        shape_out[0] = len(groups)
        array_out = np.ndarray(shape_out, dtype=array.dtype)

        if type(groups) == list:
            # Make a dictionary
            groups = {{n: groups[n] for n in range(len(groups))}}

        if not grouped_args is None:
            kwargs0 = copy.deepcopy(kwargs)
        for n in range(len(groups)):
            k = list(groups.keys())[n]

            # Add additional arguments that need to be grouped to kwargs
            if not grouped_args is None:
                kwargs = copy.deepcopy(kwargs0)
                for ka, v in grouped_args.items():
                    kwargs[ka] = v[groups[k], ...]

            array_out[n, ...] = np.apply_along_axis(func, 0, array[groups[k],
                                                                   ...], *args,
                                                    **kwargs)

        return array_out
