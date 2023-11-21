#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging

import numpy as np

from os import scandir
from os.path import join, basename
from netCDF4 import Dataset

from . import tools


def create_dir_and_copy_input(dest_dir, lambdas_src, maps_src):
    """Create a directory at dest_dir (**COSMO** input) and copy src there.

    lambdas_src -> dest_dir/lambdas.nc
    maps_src   -> dest_dir/maps.nc

    Parameters
    ----------
    dest_dir : str
    lambdas_src : str
    maps_src : str
    """
    tools.create_dir(dest_dir, "input data for OCTE")
    tools.copy_file(lambdas_src, join(dest_dir, 'lambdas.nc'))
    tools.copy_file(maps_src, join(dest_dir, 'maps.nc'))


def read_lambdas(nc_dataset):
    """Read the lambda values from the nc_dataset.

    The relevant lambda values are the last ones along the
    dimension ~nparam~. They are returned in a list of length
    ~nensembles~.

    Parameters
    ----------
    nc_dataset : netCDF4.Dataset
        Contains a variable called ~lambda~ with dimensions,
        ~(nensembles, nparam)~

    Returns
    -------
    list(float)
    """
    lambda_var = nc_dataset['lambda']
    assert len(lambda_var.dimensions) == 2, (
        "Number of dimensions of 'lambda' variable doesn't match")
    # this would not be a problem in python, but COSMO breaks if
    # this doesn't hold:
    assert lambda_var.dimensions[1] == 'nparam', (
        "Dimension ordering of 'lambda' variable incorrect")

    return list(lambda_var[:, -1])


def perturb_bg_in_dataset(nc_dataset, lambdas):
    """Create BG fields from lambdas in nc_dataset.

    Perturb the background CO2 field in nc_dataset based on lambda values in
    lambdas.
    The pertubation is just a global scaling of the field by one
    of the lambda values. For each ensemble member (=each lambda), a new
    variable is created.

    The perturbed fields are created by multiplying the values of
    a variable called ~CO2_BG~ with one of the lambda-values. The new
    field is stored in a newly created variable called ~CO2_BGnnn~,
    where ~nnn~ runs from ~001~ to ~{nensembles:03d}~.

    If there is no variable ~CO2_BG~ in nc_dataset, do nothing.

    Parameters
    ----------
    nc_dataset : netCDF4.Dataset
        If this doesn't contain a variable ~CO2_BG~, do nothing. If it does,
        create a new variable for each lambda value with the original field
        scaled.
    lambdas : list(float)
        For each value in the list, create an ensemble member
    """
    basevar_name = 'CO2_BG'
    filename = basename(nc_dataset.filepath())

    if basevar_name not in nc_dataset.variables:
        logging.info("No BG in dataset {}, doing nothing".format(filename))
        return

    basevar = nc_dataset[basevar_name]

    field = np.array(basevar[:])
    dtype = basevar.datatype
    dims = basevar.dimensions
    attr_names = basevar.ncattrs()
    try:
        # Remove fill value from ncattrs because setting it after
        # construction is not possible
        attr_names.remove('_FillValue')
        fill_value = basevar.getncattr('_FillValue')
    except ValueError:
        fill_value = None

    attrs = dict((name, basevar.getncattr(name)) for name in attr_names)

    for i, lambda_ in enumerate(lambdas):
        name = basevar_name + '{:03d}'.format(i + 1)
        var = nc_dataset.createVariable(varname=name,
                                        datatype=dtype,
                                        dimensions=dims,
                                        fill_value=fill_value)
        var[:] = field * lambda_
        for attrname, attrval in attrs.items():
            var.setncattr(name=attrname, value=attrval)

    logging.info("Created BG ensemble in dataset {}".format(filename))


def perturb_bgs_in_dir(lambdas_nc, directory):
    """Create perturbed BG fields from read lambda values in all NetCDF-files
    in the directory.

    Perturb the background CO2 field in all the NetCDF-files
    found in directory based on lambda values from lambdas_nc.
    The pertubation is just a global scaling of the field by one
    lambda value. For each ensemble, a new variable is created.

    Lambda values and the number of ensembles are read from the file
    lambdas_nc. The lambda-value for the BG is assumed to be the last
    value along the ~nparam~-dimension of the ~lambda~-variable.

    The perturbed fields are then created by multiplying the values of
    a variable called ~CO2_BG~ with one of the lambda-values. The new
    field is stored in a newly created variable called ~CO2_BGnnn~,
    where ~nnn~ runs from ~001~ to ~{nensembles:03d}~

    Parameters
    ----------
    lambdas_nc : str
        Path to the file storing the lambda-values.
        Contains a variable called ~lambda~ with dimensions,
        ~(nensembles, nparam)~.
    directory : str
        Path to the output-dir of int2lm. Add perturbed BG-fields to all
        netcdf-files there containing a variable called ~CO2_BG~.

    """
    with Dataset(lambdas_nc) as lambdas_file:
        lambdas = read_lambdas(lambdas_file)

    with scandir(directory) as dir_iterator:
        for entry in dir_iterator:
            if not entry.name.startswith('.') and entry.is_file():
                try:
                    with Dataset(entry.path, 'a') as nc_dataset:
                        perturb_bg_in_dataset(nc_dataset, lambdas)
                except OSError:
                    logging.info("File {} is not a netCDF-file.".format(
                        entry.name))


def main(cfg, model_cfg):
    """Copy necessary input files for **COSMO** and perturb BG.

    Copies the NetCDF-files found at ``cfg.octe_maps`` and ``cfg.octe_lambdas`` to
    the **COSMO** input-directory.

    Perturbs the background tracer field. To do that, it reads the lambda-value
    from the ``cfg.octe_lambdas`` (last value along the nparam-dimension) and
    scales the BG-field produced by int2lm, creating a new variable for each
    ensemble.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    model_cfg : dict
        Model configuration settings loaded from the ``config/models.yaml`` file.
    """
    dest_dir = join(cfg.cosmo_input, 'octe')
    create_dir_and_copy_input(dest_dir=dest_dir,
                              lambdas_src=cfg.octe_lambdas,
                              maps_src=cfg.octe_maps)
    logging.info("Copied OCTE files {} and {} to {}".format(
        cfg.octe_lambdas, cfg.octe_maps, dest_dir))

    logging.info("Starting to create BG-ensembles in " + cfg.int2lm_output)
    perturb_bgs_in_dir(cfg.octe_lambdas, cfg.int2lm_output)
    logging.info("Finished creating BG-ensembles")
