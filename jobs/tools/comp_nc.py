#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Compare two netCDF-files

# Author: dao, david.ochsner@empa.ch

import argparse
import netCDF4 as nc
import numpy as np

colors = {"green": '\033[32m', "red": '\033[31m', "yellow": '\033[33m'}


def ccprint(text, color=None, verbose=True):
    """Print-wrapper that Conditionally prints Colored text

    Parameters
    ----------
    text : str
        Text to print
    color : str
        Color of the text
        One of 'red','green' or 'yellow', or 'None' for black
    verbose : bool
        If 'True' the text is printed, if 'False' nothing happens
    """
    if not verbose:
        return

    if color is not None:
        try:
            print(colors[color] + text + '\033[0m')
        except KeyError:
            raise ValueError("Unrecognized color")
    else:
        print(text)


def import_data(path):
    """Imports the data at path into a netCDF4-Dataset

    Parameters
    ----------
    path : str
        Path to the .nc file
    """
    return nc.Dataset(path)


def datasets_equal(dataset1, dataset2, variables, verbose=True):
    """Compare the contents of dataset1 and dataset2

    Compare with numpy.isclose whether the two datasets are equal. No check for
    equality (of the values or bitwise of the files) is performed, as numerical
    errors can produce slightly different files for essentially identical
    computations. Rather, the values are compared to absolute and relative
    tolerances, check np.isclose documentation for more detail.

    If variables is not empty, only the provided variables are compared.

    Parameters
    ----------
    dataset1 : netCDF4.Dataset
    dataset2 : netCDF4.Dataset
    variables : list of str
        List of the variables to be compared. If it is empty, all variables
        are compared.
    verbose : bool
        If True, results will be printed to stdout.
    Returns
    -------
    bool
        True if the datasets, or if provided the selected variables, are equal,
        False otherwise.
    """
    if not variables:
        variables = set(dataset1.variables.keys())
        variables2 = set(dataset2.variables.keys())

        if not variables == variables2:
            ccprint("Files don't contain the same variables.", "red", verbose)
            ccprint("The following variables are in only "
                    "one of the files:", None, verbose)
            ccprint(variables.symmetric_difference(variables2), None, verbose)
            ccprint("The common variables are:", None, verbose)
            ccprint(variables.intersection(variables2), None, verbose)
            return False
    else:
        assert set(dataset1.variables.keys()).issuperset(variables), (
            "Dataset 1 doesn't contain all variables that should be compared")
        assert set(dataset2.variables.keys()).issuperset(variables), (
            "Dataset 2 doesn't contain all variables that should be compared")

    result = True
    for var in variables:
        if not dataset1[var].dtype == dataset2[var].dtype:
            ccprint("{} has different types.".format(var), "red", verbose)
            result = False

        if (dataset1[var].dtype in np.sctypes['float']
                or dataset1[var].dtype in np.sctypes['int']):
            if np.allclose(dataset1[var], dataset2[var]):
                ccprint("{} is equal.".format(var), None, verbose)
            else:
                ccprint("{} is not equal".format(var), "red", verbose)
                result = False
        else:
            ccprint(
                "{} is not a numeric type "
                "and not compared.".format(var), None, verbose)

    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser("Compare two netCDF files.")
    parser.add_argument("Dataset1",
                        type=str,
                        help="Path to the first dataset.")
    parser.add_argument("Dataset2",
                        type=str,
                        help="Path to the second dataset.")
    parser.add_argument("-v",
                        "--variables",
                        nargs='*',
                        default=[],
                        dest="variables",
                        help="Variables to be compared. If "
                        "none are given, all variables in the files are "
                        "compared.")
    args = parser.parse_args()

    if datasets_equal(import_data(args.Dataset1), import_data(args.Dataset2),
                      args.variables):
        ccprint("Provided files are equal", color="green")
    else:
        ccprint("Provided files are not equal", color="red")
