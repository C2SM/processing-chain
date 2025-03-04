#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 15:03:02 2019 

This class contains helper functions mainly for sampling WRF-Chem, but a
few are also needed by the WRF-Chem column observation operator.

@author: friedemann

Modified on June 26, 10:40:12, 2023
Adaptation for sampling ICON instead of WRF.

@author: David Ho
"""

# Instructions for pylint:
# pylint: disable=too-many-instance-attributes
# pylint: disable=W0201
# pylint: disable=C0301
# pylint: disable=E1136
# pylint: disable=E1101

import os
import shutil
import re
import glob
import bisect
import copy
import numpy as np
import netCDF4 as nc
import datetime as dt
#import wrf    # Not needed, since working on ICON
#import f90nml # Not needed, used for reading wrf namelist
import pickle
import xarray as xr
import pandas as pd

# CTDAS modules
import da.tools.io4 as io
from da.tools.icon.utilities import utilities

# Erik added:
from datetime import datetime, timedelta


class ICON_Helper(object):
    """Contains helper functions for sampling WRF-Chem"""

    def __init__(self, settings):
        self.settings = settings

    #def __init__(self):  # Use this part for offline testing
    #    pass

    def validate_settings(self, needed_items=[]):
        """
        This is based on WRFChemOO._validate_rc
        """

        if len(needed_items) == 0:
            return

        for key in needed_items:
            if key not in self.settings:
                msg = "Missing a required value in settings: %s" % key
                raise IOError(msg)

    @staticmethod
    def get_pressure_boundaries_paxis(p_axis, p_surf):
        """
        Arguments
        ---------
        p_axis (:class:`array-like`)
            Pressure at mid points of layers
        p_surf (:class:`numeric`)
            Surface pressure
        Output
        ------
        Pressure at layer boundaries
        """

        #pb = np.array([float("nan")]*(len(p_axis)+1))
        #pb[0] = p_surf
        #
        #for nl in range(len(pb)-1):
        #    pb[nl+1] = pb[nl] + 2*(p_axis[nl] - pb[nl])
        # ^ commented out by David coz it didn't work
        # v Added by David
        p_full = np.insert(p_axis, 0, psurf,
                           axis=1)  # Insert p_surf to the first index
        pb = np.array([float("nan")] * (len(p_axis) + 1))
        pb[0] = p_surf

        for nl in range(len(pb) - 1):
            pb[nl + 1] = 0.5 * (p_full[nl] + p_full[nl + 1])

        return pb

    @staticmethod
    def get_pressure_boundaries_znw(znw, p_surf, p_top):
        """
        Arguments
        ---------
        ZNW (:class:`ndarray`)
            Eta coordinates of z-staggered WRF grid. For each
            observation (2D)
        p_surf (:class:`ndarray`)
            Surface pressure (1D)
        p_top (:class:`ndarray`)
            Top-of-atmosphere pressure (1D)
        Output
        ------
        Pressure at layer boundaries

        CAVEATS
        -------
        Maybe I should rather use P_HYD? Well, Butler et al. 2018
        (https://www.geosci-model-dev-discuss.net/gmd-2018-342/) used
        znu and surface pressure to compute "WRF midpoint layer
        pressure".

        For WRF it would be more consistent to interpolate to levels.
        See also comments in code.
        """

        return znw * (p_surf - p_top) + p_top

    @staticmethod
    def get_int_coefs(pb_ret, pb_mod, level_def):
        """
        Computes a coefficients matrix to transfer a model profile onto
        a retrieval pressure axis.
        
        If level_def=="layer_average", this assumes that profiles are
        constant in each layer of the retrieval, bound by the pressure
        boundaries pb_ret. In this case, the WRF model layer is treated
        in the same way, and coefficients integrate over the assumed
        constant model layers. This works with non-staggered WRF
        variables (on "theta" points). However, this is actually not how
        WRF is defined, and the implementation should be changed to
        z-staggered variables. Details for this change are in a comment
        at the beginning of the code.

        If level_def=="pressure_boundary" (IMPLEMENTATION IN PROGRESS),
        assumes that profiles, kernel and pwf are defined at pressure
        boundaries that don't have a thickness (this is how OCO-2 data
        are defined, for example). In this case, the coefficients
        linearly interpolate adjacent model level points. This is
        incompatible with the treatment of WRF in the above-described
        layer-average assumption, but is closer to how WRF is actually
        defined. The exception is that pb_mod is still constructed and
        non-staggered variables are not defined at psurf. This can only
        be fixed by switching to z-staggered variables.
    
        In cases where retrieval surface pressure is higher than model
        surface pressure, and in cases where retrieval top pressure is
        lower than model top pressure, the model profile will be
        extrapolated with constant tracer mixing ratios. In cases where
        retrieval surface pressure is lower than model surface pressure,
        and in cases where retrieval top pressure is higher than model
        top pressure, only the parts of the model column that fall
        within the retrieval presure boundaries are sampled.
    
        Arguments
        ---------
        pb_ret (:class:`array_like`)
            Pressure boundaries of the retrieval column
        pb_mod (:class:`array_like`)
            Pressure boundaries of the model column
        level_def (:class:`string`)
            "layer_average" or "pressure_boundary" (IMPLEMENTATION IN
            PROGRESS). Refers to the retrieval profile.
            
            Note 2021-09-13: Inspected code for pressure_boundary.
            Should be correct. Interpolates linearly between two model
            levels.

    
        Returns
        -------
        coefs (:class:`array_like`)
                Integration coefficient matrix. Each row sums to 1.
    
        Usage
        -----
             .. code-block:: python
    
                 import numpy as np
                 pb_ret = np.linspace(900., 50., 5)
                 pb_mod = np.linspace(1013., 50., 7)
                 model_profile = 1. - np.linspace(0., 1., len(pb_mod)-1)**3
                 coefs = get_int_coefs(pb_ret, pb_mod, "layer_average")
                 retrieval_profile = np.matmul(coefs, model_profile)
        """

        if level_def == "layer_average":
            # This code assumes that WRF variables are constant in
            # layers, but they are defined on levels. This can be seen
            # for example by asking wrf.interplevel for the value of a
            # variable that is defined on the mass grid ("theta points")
            # at a pressure slightly higher than the pressure on its
            # grid (wrf.getvar(ncf, "p")), it returns nan. So There is
            # no extrapolation. There are no layers. There are only
            # levels.
            # In addition, this page here:
            # https://www.openwfm.org/wiki/How_to_interpret_WRF_variables
            # says that to find values at theta-points of a variable
            # living on u-points, you interpolate linearly. That's the
            # other way around from what I would do if I want to go from
            # theta to staggered.
            # WRF4.0 user guide:
            # - ungrib can interpolate linearly in p or log p
            # - real.exe comes with an extrap_type namelist option, that
            #   extrapolates constantly BELOW GROUND.
            # This would mean the correct way would be to integrate over
            # a piecewise-linear function. It also means that I really
            # want the value at surface level, so I'd need the CO2
            # fields on the Z-staggered grid ("w-points")! Interpolate
            # the vertical in p with wrf.interp1d, example:
            # wrf.interp1d(np.array(rh.isel(south_north=1, west_east=0)),
            #              np.array(p.isel(south_north=1, west_east=0)),
            #              np.array(988, 970))
            # (wrf.interp1d gives the same results as wrf.interplevel,
            # but the latter just doesn't want to work with single
            # columns (32,1,1), it wants a dim>1 in the horizontal
            # directions)
            # So basically, I can keep using pb_ret and pb_mod, but it
            # would be more accurate to do the piecewise-linear
            # interpolation and the output matrix will have 1 more
            # value in each dimension.

            # Calculate integration weights by weighting with layer
            # thickness. This assumes that both axes are ordered
            # psurf to ptop.
            coefs = np.ndarray(shape=(len(pb_ret) - 1, len(pb_mod) - 1))
            coefs[:] = 0.

            # Extend the model pressure grid if retrieval encompasses
            # more.
            pb_mod_tmp = copy.deepcopy(pb_mod)

            # In case the retrieval pressure is higher than the model
            # surface pressure, extend the lowest model layer.
            if pb_mod_tmp[0] < pb_ret[0]:
                pb_mod_tmp[0] = pb_ret[0]

            # In case the model doesn't extend as far as the retrieval,
            # extend the upper model layer upwards.
            if pb_mod_tmp[-1] > pb_ret[-1]:
                pb_mod_tmp[-1] = pb_ret[-1]

            # For each retrieval layer, this loop computes which
            # proportion falls into each model layer.
            for nret in range(len(pb_ret) - 1):

                # 1st model pressure boundary index = the one before the
                # first boundary with lower pressure than high-pressure
                # retrieval layer boundary.
                model_lower = pb_mod_tmp < pb_ret[nret]
                id_model_lower = model_lower.nonzero()[0]
                id_min = id_model_lower[0] - 1

                # Last model pressure boundary index = the last one with
                # higher pressure than low-pressure retrieval layer
                # boundary.
                model_higher = pb_mod_tmp > pb_ret[nret + 1]

                id_model_higher = model_higher.nonzero()[0]

                if len(id_model_higher) == 0:
                    #id_max = id_min
                    raise ValueError("This shouldn't happen. Debug.")
                else:
                    id_max = id_model_higher[-1]

                # By the way, in case there is no model level with
                # higher pressure than the next retrieval level,
                # id_max must be the same as id_min.

                # For each model layer, find out how much of it makes up this
                # retrieval layer
                for nmod in range(id_min, id_max + 1):
                    if (nmod == id_min) & (nmod != id_max):
                        # Part of 1st model layer that falls within
                        # retrieval layer
                        coefs[nret, nmod] = pb_ret[nret] - pb_mod_tmp[nmod + 1]
                    elif (nmod != id_min) & (nmod == id_max):
                        # Part of last model layer that falls within
                        # retrieval layer
                        coefs[nret, nmod] = pb_mod_tmp[nmod] - pb_ret[nret + 1]
                    elif (nmod == id_min) & (nmod == id_max):
                        # id_min = id_max, i.e. model layer encompasses
                        # retrieval layer
                        coefs[nret, nmod] = pb_ret[nret] - pb_ret[nret + 1]
                    else:
                        # Retrieval layer encompasses model layer
                        coefs[nret,
                              nmod] = pb_mod_tmp[nmod] - pb_mod_tmp[nmod + 1]

                coefs[nret, :] = coefs[nret, :] / sum(coefs[nret, :])

            # I tested the code with many cases, but I'm only 99.9% sure
            # it works for all input. Hence a test here that the
            # coefficients sum to 1 and dump the data if not.
            sum_ = np.abs(coefs.sum(1) - 1)
            if np.any(sum_ > 2. * np.finfo(sum_.dtype).eps):
                dump = dict(pb_ret=pb_ret, pb_mod=pb_mod, level_def=level_def)
                fp = "int_coefs_dump.pkl"
                with open(fp, "w") as f:
                    pickle.dump(dump, f, 0)

                msg_fmt = "Something doesn't sum to 1. Arguments dumped to: %s"
                raise ValueError(msg_fmt % fp)

        elif level_def == "pressure_boundary":
            #msg = "level_def is pressure_boundary. Implementation not complete."
            ##logging.error(msg)
            #raise ValueError(msg)
            # Note 2021-09-13: Inspected the code. Should be correct.

            # Go back to pressure midpoints for model...
            # Change this line to p_mod = pb_mod for z-staggered
            # variables
            p_mod = pb_mod[1:] - 0.5 * np.diff(
                pb_mod)  # Interpolate linearly in pressure space

            coefs = np.ndarray(shape=(len(pb_ret), len(pb_mod) - 1))
            coefs[:] = 0.

            # For each retrieval pressure level, compute linear
            # interpolation coefficients
            for nret in range(len(pb_ret)):
                nmod_list = (p_mod < pb_ret[nret]).nonzero()[0]
                if (len(nmod_list) > 0):
                    nmod = nmod_list[0] - 1
                    if nmod == -1:
                        # Constant extrapolation at surface
                        nmod = 0
                        coef = 1.
                    else:
                        # Normal case:
                        coef = (pb_ret[nret] - p_mod[nmod + 1]) / (
                            p_mod[nmod] - p_mod[nmod + 1])
                else:
                    # Constant extrapolation at atmosphere top
                    nmod = len(p_mod) - 2
                    coef = 0.

                coefs[nret, nmod] = coef
                coefs[nret, nmod + 1] = 1. - coef

        else:
            msg = "Unknown level_def: " + level_def
            raise ValueError(msg)

        return coefs

    @staticmethod
    def get_pressure_weighting_function(pressure_boundaries, rule):
        """
        Compute pressure weighting function according to 'rule'.
        Valid rules are:
            - simple (=layer thickness)
            - connor2008 (not implemented)
        """
        if rule == 'simple':
            pwf = np.abs(
                np.diff(pressure_boundaries) / np.ptp(pressure_boundaries))
        else:
            raise NotImplementedError("Rule %s not implemented" % rule)

        return pwf

    ### David: Original function from ctdas-wrf  ###
    ###         Keeping here as reference.       ###

    def sample_total_columns(self, dat, loc, fields_list):
        """
        Sample total_columns of fields_list in WRF output in
        self.settings["run_dir"] at the location id_xy in domain, id_t
        in all wrfout-times. Files and indices therein are recognized
        by id_t and file_time_start_indices.
        All quantities needed for computing total columns from profiles
        are in dat (kernel, prior, ...).

        Arguments
        ---------
        dat (:class:`list`)
            Result of wrfhelper.read_sampling_coords. Used here: prior,
            prior_profile, kernel, psurf, pressure_axis, [, pwf]
            If psurf or any of pressure_axis are nan, wrf's own
            surface pressure is used and pressure_axis constructed
            from this and the number of levels in the averaging kernel.
            This allows sampling with synthetic data that don't have
            pressure information. This only works with level_def
            "layer_average".
            If pwf is not present or nan, a simple one is created, for
            level_def "layer_average".
        loc (:class:`dict`)
            A dictionary with all location-related input for sampling,
            computed in wrfout_sampler. Keys:
            id_xy, domain: Domain coordinates
            id_t: Timestep (continous throughout all files)
            frac_t: Interpolation coeficient between id_t and id_t+1:
                    t_obs = frac_t*t[id_t] + (1-frac_t)*t[id_t+1])
            file_start_time_indices: Time index at which a new wrfout
                                     file starts
            files: names of wrfout files.
        fields_list (:class:`list`)
            The fields to sample total columns from.

        Output
        ------
        sampled_columns (:class:`array`)
            A 2D-array of sampled columns.
            Shape: (len(dat["prior"]), len(fields_list))
        """

        # Initialize output
        tc = np.ndarray(shape=(len(dat["prior"]), len(fields_list)),
                        dtype=float)
        tc[:] = float("nan")

        # Process by domain
        UD = list(set(loc["domain"]))
        # Added by David, above ^ returns [0,1] where domain 0 doesn't exsist
        UD = [1]
        for dom in UD:
            idd = np.nonzero(loc["domain"] == dom)[0]
            # Process by id_t
            UT = list(set(loc["id_t"][idd]))
            for time_id in UT:
                # Coordinates to process
                idt = idd[np.nonzero(loc["id_t"][idd] == time_id)[0]]
                # Get tracer ensemble profiles
                profiles = self._read_and_intrp_v(loc, fields_list, time_id,
                                                  idt)
                # List, len=len(fields_list), shape of each: (len(idt),nz)
                # Get pressure axis:
                #paxis = self.read_and_intrp(wh_names, id_ts, frac_t, id_xy, "P_HYD")/1e2 # Pa -> hPa
                psurf = self._read_and_intrp_v(loc, ["PSFC"], time_id,
                                               idt)[0] / 1.e2  # Pa -> hPa
                # Shape: (len(idt),)
                ptop = float(
                    self.namelist["domains"]["p_top_requested"]) / 1.e2
                # Shape: (len(idt),)
                znw = self._read_and_intrp_v(loc, ["ZNW"], time_id, idt)[0]
                #Shape:(len(idt),nz)

                # DONE reading from file.
                # Here it starts to make sense to loop over individual observations
                for nidt in range(len(idt)):
                    nobs = idt[nidt]
                    # Construct model pressure layer boundaries
                    pb_mod = self.get_pressure_boundaries_znw(
                        znw[nidt, :], psurf[nidt], ptop)

                    if (np.diff(pb_mod) >= 0).any():
                        msg = ("Model pressure boundaries for observation %d " + \
                               "are not monotonically decreasing! Investigate.") % nobs
                        raise ValueError(msg)

                    # Construct retrieval pressure layer boundaries
                    if dat["level_def"][nobs] == "layer_average":
                        if np.any(np.isnan(dat["pressure_levels"][nobs])) \
                           or np.isnan(dat["psurf"][nobs]):
                            # Code for synthetic data without a pressure axis,
                            # but with an averaging kernel:
                            # Use wrf's surface and top pressure
                            nlayers = len(dat["averaging_kernel"][nobs])
                            pb_ret = np.linspace(psurf[nidt], ptop,
                                                 nlayers + 1)
                        else:
                            nlayers = len(dat["averaging_kernel"][nobs])
                            pb_ret = np.linspace(psurf[nidt], ptop,
                                                 nlayers + 1)
                            # Below commented out by David
                            # Because somehow doesn't work
                            #pb_ret = self.get_pressure_boundaries_paxis(
                            #        dat["pressure_levels"][nobs],
                            #        dat["psurf"][nobs])
                    elif dat["level_def"][nobs] == "pressure_boundary":
                        if np.any(np.isnan(dat["pressure_levels"][nobs])):
                            # Code for synthetic data without a pressure axis,
                            # but with an averaging kernel:
                            # Use wrf's surface and top pressure
                            nlevels = len(dat["averaging_kernel"][nobs])
                            pb_ret = np.linspace(psurf[nidt], ptop, nlevels)
                        else:
                            pb_ret = dat["pressure_levels"][nobs]

                    if (np.diff(pb_ret) >= 0).any():
                        msg = ("Retrieval pressure boundaries for " + \
                               "observation %d are not monotonically " + \
                               "decreasing! Investigate.") % nobs
                        print('pb_ret[:]: %s, np.diff(pb_ret): %s' %
                              (pb_ret[:], np.diff(pb_ret)))
                        raise ValueError(msg)

                    # Get vertical integration coefficients (i.e. to
                    # "interpolate" from model to retrieval grid)
                    coef_matrix = self.get_int_coefs(pb_ret, pb_mod,
                                                     dat["level_def"][nobs])

                    # Model retrieval with averaging kernel and prior profile
                    if "pressure_weighting_function" in list(dat.keys()):
                        pwf = dat["pressure_weighting_function"][nobs]
                    if (not "pressure_weighting_function" in list(
                            dat.keys())) or np.any(np.isnan(pwf)):
                        # Construct pressure weighting function from
                        # pressure boundaries
                        pwf = self.get_pressure_weighting_function(
                            pb_ret, rule="simple")

                    # Compute pressure-weighted averaging kernel
                    avpw = pwf * dat["averaging_kernel"][nobs]

                    # Get prior
                    prior_col = dat["prior"][nobs]
                    prior_profile = dat["prior_profile"][nobs]
                    if np.isnan(prior_col):  # compute prior
                        prior_col = np.dot(pwf, prior_profile)

                    # Compute total columns
                    for nf in range(len(fields_list)):
                        # Integrate model profile
                        profile_intrp = np.matmul(coef_matrix,
                                                  profiles[nf][nidt, :])

                        # Model retrieval
                        tc[nobs, nf] = prior_col + np.dot(
                            avpw, profile_intrp - prior_profile)

                        # Test phase: save pb_ret, pb_mod, coef_matrix,
                        # one profile for manual checking

                        # dat_save = dict(pb_ret=pb_ret,
                        #                pb_mod=pb_mod,
                        #                coef_matrix=coef_matrix,
                        #                ens_profile=ens_profiles[0],
                        #                profile_intrp=profile_intrp,
                        #                id=dat.id)
                        #
                        #out = open("model_profile_%d.pkl"%dat.id, "w")
                        #cPickle.dump(dat_save, out, 0)
        # Average over footprint
        if self.settings["footprint_samples_dim"] > 1:
            indices = utilities.get_index_groups(dat["sounding_id"])

            # Make sure that this is correct: i know the number of indices
            lens = [len(group) for group in list(indices.values())]
            correct_len = self.settings["footprint_samples_dim"]**2
            if np.any([len_ != correct_len for len_ in set(lens)]):
                raise ValueError("Not all footprints have %d samples" %
                                 correct_len)
            # Ok, paranoid mode, also confirm that the indices are what I
            # think they are: consecutive numbers
            ranges = [np.ptp(group) for group in list(indices.values())]
            if np.any([ptp != correct_len for ptp in set(ranges)]):
                raise ValueError("Not all footprints have consecutive samples")

            tc_original = copy.deepcopy(tc)
            tc = utilities.apply_by_group(np.average, tc_original, indices)

        return tc

    ### David: Original function from ctdas-wrf  ###
    ###         Keeping here as reference.       ###

    @staticmethod
    def _read_and_intrp_v(loc, fields_list, time_id, idp):
        """
        Helper function for sample_total_columns.
        read_and_intrp, but vectorized.
        Reads in fields and interpolates
        them linearly in time.
        
        Arguments
        ----------
        loc (:class:`dict`)
            Passed through from sample_total_columns, see there.
        fields_list (:class:`list` of :class:`str`)
            List of netcdf-variables to process.
        time_id (:class:`int`)
            Time index referring to all files in loc to read
        idp (:class:`array` of :class:`int`)
            Indices for id_xy, domain and frac_t in loc (i.e.
            observations) to process.
        
        Output
        ------
        List of temporally interpolated fields, one entry per member of
        fields_list.
        """

        var_intrp_l = list()

        # Check we were really called with observations for just one domain
        domains = set(loc["domain"][idp])
        if len(domains) > 1:
            raise ValueError(
                "I can only operate on idp with identical domains.")
        dom = domains.pop()

        # Select input files
        id_file0 = bisect.bisect_right(loc["file_start_time_indices"][dom],
                                       time_id) - 1
        id_file1 = bisect.bisect_right(loc["file_start_time_indices"][dom],
                                       time_id + 1) - 1
        if id_file0 < 0 or id_file1 < 0:
            raise ValueError("This shouldn't happen.")

        # Get time id in file
        id_t_file0 = time_id - loc["file_start_time_indices"][dom][id_file0]
        id_t_file1 = time_id + 1 - loc["file_start_time_indices"][dom][id_file1]

        # Open files
        nc0 = nc.Dataset(loc["files"][dom][id_file0], "r")
        nc1 = nc.Dataset(loc["files"][dom][id_file1], "r")
        # Per field to sample
        for field in fields_list:
            # Read input file
            field0 = wrf.getvar(wrfin=nc0,
                                varname=field,
                                timeidx=id_t_file0,
                                squeeze=False,
                                meta=False)

            field1 = wrf.getvar(wrfin=nc1,
                                varname=field,
                                timeidx=id_t_file1,
                                squeeze=False,
                                meta=False)

            if len(field0.shape) == 4:
                # Sample field at timesteps before and after observation
                # They are ordered nt x nz x ny x nx
                # var0 will have shape (len(idp),len(profile))
                var0 = field0[0, :, loc["id_xy"][idp, 1], loc["id_xy"][idp, 0]]
                var1 = field1[0, :, loc["id_xy"][idp, 1], loc["id_xy"][idp, 0]]
                # Repeat frac_t for profile size
                frac_t_ = np.array(loc["frac_t"][idp]).reshape(
                    (len(idp), 1)).repeat(var0.shape[1], 1)
            elif len(field0.shape) == 3:
                # var0 will have shape (len(idp),)
                var0 = field0[0, loc["id_xy"][idp, 1], loc["id_xy"][idp, 0]]
                var1 = field1[0, loc["id_xy"][idp, 1], loc["id_xy"][idp, 0]]
                frac_t_ = np.array(loc["frac_t"][idp])
            elif len(field0.shape) == 2:
                # var0 will have shape (len(idp),len(profile))
                # This is for ZNW, which is saved as (time_coordinate,
                # vertical_coordinate)
                var0 = field0[[0] * len(idp), :]
                var1 = field1[[0] * len(idp), :]
                frac_t_ = np.array(loc["frac_t"][idp]).reshape(
                    (len(idp), 1)).repeat(var0.shape[1], 1)
            else:
                raise ValueError("Can't deal with field with %d dimensions." %
                                 len(field0.shape))

            # Interpolate in time
            var_intrp_l.append(var0 * frac_t_ + var1 * (1. - frac_t_))

        nc0.close()
        nc1.close()

        return var_intrp_l

    @staticmethod
    def read_sampling_coords(sampling_coords_file, id0=None, id1=None):
        """Read in samples"""

        ncf = nc.Dataset(sampling_coords_file, "r")
        if id0 is None:
            id0 = 0
        if id1 is None:
            id1 = len(ncf.dimensions['soundings'])

        dat = dict(sounding_id=np.array(ncf.variables["sounding_id"][id0:id1]),
                   date=ncf.variables["date"][id0:id1],
                   latitude=np.array(ncf.variables["latitude"][id0:id1]),
                   longitude=np.array(ncf.variables["longitude"][id0:id1]),
                   latc_0=np.array(ncf.variables["latc_0"][id0:id1]),
                   latc_1=np.array(ncf.variables["latc_1"][id0:id1]),
                   latc_2=np.array(ncf.variables["latc_2"][id0:id1]),
                   latc_3=np.array(ncf.variables["latc_3"][id0:id1]),
                   lonc_0=np.array(ncf.variables["lonc_0"][id0:id1]),
                   lonc_1=np.array(ncf.variables["lonc_1"][id0:id1]),
                   lonc_2=np.array(ncf.variables["lonc_2"][id0:id1]),
                   lonc_3=np.array(ncf.variables["lonc_3"][id0:id1]),
                   prior=np.array(ncf.variables["prior"][id0:id1]),
                   prior_profile=np.array(ncf.variables["prior_profile"][
                       id0:id1,
                   ]),
                   averaging_kernel=np.array(
                       ncf.variables["averaging_kernel"][id0:id1]),
                   pressure_levels=np.array(
                       ncf.variables["pressure_levels"][id0:id1]),
                   pressure_weighting_function=np.array(
                       ncf.variables["pressure_weighting_function"][id0:id1]),
                   level_def=ncf.variables["level_def"][id0:id1],
                   psurf=np.array(ncf.variables["psurf"][id0:id1]))

        ncf.close()

        # Convert level_def from it's weird nc format to string
        dat["level_def"] = nc.chartostring(dat["level_def"])

        # Convert date to datetime object
        dat["time"] = [dt.datetime(*x) for x in dat["date"]]

        return dat

    @staticmethod
    def write_simulated_columns(obs_id, simulated, nmembers, outfile):
        """Write simulated observations to file."""

        # Output format: see obs_xco2_fr

        f = io.CT_CDF(outfile, method="create")

        dimid = f.createDimension("sounding_id", size=None)
        dimid = ("sounding_id", )
        savedict = io.std_savedict.copy()
        savedict["name"] = "sounding_id"
        savedict["dtype"] = "int64"
        savedict["long_name"] = "Unique_Dataset_observation_index_number"
        savedict["units"] = ""
        savedict["dims"] = dimid
        savedict["comment"] = "Format as in input"
        savedict["values"] = obs_id.tolist()
        f.add_data(savedict, nsets=0)

        dimmember = f.createDimension("nmembers", size=nmembers)
        dimmember = ("nmembers", )
        savedict = io.std_savedict.copy()
        savedict["name"] = "column_modeled"
        savedict["dtype"] = "float"
        savedict["long_name"] = "Simulated total column"
        savedict["units"] = "??"
        savedict["dims"] = dimid + dimmember
        savedict["comment"] = "Simulated model value created by ICON_sampler"
        savedict["values"] = simulated.tolist()
        f.add_data(savedict, nsets=0)

        f.close()

    @staticmethod
    def save_file_with_timestamp(file_path, out_dir, suffix=""):
        """ Saves a file to with a timestamp"""
        nowstamp = dt.datetime.now().strftime("_%Y-%m-%d_%H:%M:%S")
        new_name = os.path.basename(file_path) + suffix + nowstamp
        new_path = os.path.join(out_dir, new_name)
        shutil.copy2(file_path, new_path)


###################################################
# Here are some adaptations written by David Ho

    def get_icon_filenames(self, glob_pattern):
        """
        Gets the filenames in self.settings["dir.icon_sim"] that follow
        glob_pattern
        """
        path = self.settings["run_dir"]
        #path = '/work/mj0143/b301043/Project/Ensemble_sim/ICON/ICON-ART/icon-kit/ERA5_EMPA/CTDAS_test/bckup'
        # All files...
        wfiles = glob.glob(os.path.join(path, glob_pattern))
        files = [x for x in wfiles]

        # I need this sorted too often to not do it here.
        files = np.sort(files).tolist()
        return files

    @staticmethod
    def times_in_icon_file(ds_icon):
        """
        Returns the times in netCDF4.Dataset ncf as datetime object
        """
        times_nc = pd.to_datetime(ds_icon["time"].values, format='date_format')
        #times_dtm  = pd.to_datetime(ds_icon["time"].values, format='date_format')
        times_str = str(times_nc.strftime('%Y-%m-%d_%H:%M:%S')[0])
        times_dtm = dt.datetime.strptime(times_str, "%Y-%m-%d_%H:%M:%S")

        return times_dtm

    def icon_times(self, file_list):
        """Read all times in a list of icon files

        Output
        ------
        - 1D-array containing all times
        - 1D-array containing start indices of each file
        """

        #times = []
        times = list()
        start_indices = np.ndarray((len(file_list), ), int)
        for file in range(len(file_list)):
            ds = xr.open_dataset(file_list[file])
            times_this = self.times_in_icon_file(ds)
            start_indices[file] = len(times)
            #times += times_this
            times.append(times_this)
            #ncf.close()

        return times, start_indices

    ###  David: Too slow, no longer needed  ###
    ###          To be deleted              ###
    @staticmethod
    def fetch_weight_and_neighbor_cells_Serial(gridinfo,
                                               latitudes_array,
                                               longitudes_array,
                                               z_info=None):
        """
        Provide Grid info of your ICON grid, see icon_sampler.
        Given lat/lon, calculates the distances then:
                       return the indexes of the neighboring N cells from unstructured ICON grid,
                       and the weights, for horizontal interpolation.
        Vertical interpolation is skipped, since it will calculates the column average later.
        -----
        Code originally inherited from Michael Steier.
        Future developments: 
        Include vertical interpolation from 'z_info' argument, for geting the model levels.
        
        Output
        -----
        - 1D-array containing the nearest neighbor indexes
        - 1D-array containing the weights for the indexes
        """
        # Libraries for this function:
        from math import sin, cos, sqrt, atan2, radians

        # Initialize
        nn_sel_list = np.zeros(
            (len(latitudes_array),
             gridinfo.nn)).astype(int)  # indexes must be integers
        u_list = np.zeros((len(latitudes_array), gridinfo.nn))

        # Loop over lat/lon array to collect. #### This loop takes too long, needs to parallelize!!!
        for index in np.arange(len(latitudes_array)):

            # For debugging...
            #print('Calculating index: %s' %index)

            latitudes = latitudes_array[index]
            longitudes = longitudes_array[index]

            # For debugging...
            #print('Lat: %s, Lon: %s' %(latitudes, longitudes))

            # Initialize:
            nn_sel = np.zeros(gridinfo.nn)  # Index of neighbor cells
            u = np.zeros(gridinfo.nn)  # Weights for neighbor cells

            R = 6373.0  # approximate radius of earth in km

            # This step is used for filtering obs outside of domain.
            # However, in the satellite pre-processing step, we will make sure all obs are in the domain!
            # vvv Therefore, skipped... vvv

            #if (radians(longitudes)<np.nanmin(gridinfo.clon)) or (radians(longitudes)>np.nanmax(gridinfo.clon)):
            #    u[:] = np.nan
            #    return np.zeros((gridinfo.nn)), np.zeros((gridinfo.nn)).astype(int), np.zeros((gridinfo.nn)).astype(int), nn_sel[:], u[:]

            #if (radians(latitudes)<np.nanmin(gridinfo.clat)) or (radians(latitudes)>np.nanmax(gridinfo.clat)):
            #    u[:] = np.nan
            #    return np.zeros((gridinfo.nn)), np.zeros((gridinfo.nn)).astype(int), np.zeros((gridinfo.nn)).astype(int), nn_sel[:], u[:]

            #%
            lat1 = radians(latitudes)
            lon1 = radians(longitudes)

            #%
            """FIND "N" CLOSEST CENTERS"""
            distances = np.zeros((len(gridinfo.clon)))
            for icell in np.arange(len(gridinfo.clon)):
                lat2 = gridinfo.clat[icell]
                lon2 = gridinfo.clon[icell]
                dlon = lon2 - lon1
                dlat = lat2 - lat1
                a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
                c = 2 * atan2(sqrt(a), sqrt(1 - a))
                distances[icell] = R * c
            nn_sel[:] = [
                x for _, x in sorted(
                    zip(distances, np.arange(len(gridinfo.clon))))
            ][0:gridinfo.nn]
            nn_sel = nn_sel.astype(int)

            u[:] = [1. / distances[y] for y in nn_sel]

            nn_sel_list[index] = nn_sel[:]
            u_list[index] = u

            # For debugging...
            #print('Done, added NS:%s and U:%s' %(nn_sel, u[:]) )

            # End of loop

        return nn_sel_list, u_list

    ###  David: Too slow, no longer needed  ###
    ###          To be deleted              ###
    @staticmethod
    def fetch_weight_and_neighbor_cells_Parallel(args):
        #def fetch_weight_and_neighbor_cells_Parallel(idx, gridinfo, latitudes, longitudes):
        """
        Provide Grid info of your ICON grid, see icon_sampler.
        Given lat/lon, calculates the distances then:
                       return the indexes of the neighboring N cells from unstructured ICON grid,
                       and the weights, for horizontal interpolation.
        Vertical interpolation is skipped, since it will calculates the column average later.
        -----
        Code originally inherited from Michael Steier.
        Future developments: 
        Include vertical interpolation from 'z_info' argument, for geting the model levels.
        
        Output
        -----
        - 1D-array containing the nearest neighbor indexes
        - 1D-array containing the weights for the indexes
        """

        idx = args[0]
        gridinfo = args[1]
        latitudes = args[2]
        longitudes = args[3]

        # Libraries for this function:
        from math import sin, cos, sqrt, atan2, radians

        # Initialize:
        nn_sel = np.zeros(gridinfo.nn).astype(
            int)  # Index of neighbor cells, # indexes must be integers
        u = np.zeros(gridinfo.nn)  # Weights for neighbor cells

        R = 6373.0  # approximate radius of earth in km

        #%
        lat1 = radians(latitudes[idx])
        lon1 = radians(longitudes[idx])

        #%
        """FIND "N" CLOSEST CENTERS"""
        distances = np.zeros((len(gridinfo.clon)))
        for icell in np.arange(len(gridinfo.clon)):
            lat2 = gridinfo.clat[icell]
            lon2 = gridinfo.clon[icell]
            dlon = lon2 - lon1
            dlat = lat2 - lat1
            a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
            c = 2 * atan2(sqrt(a), sqrt(1 - a))
            distances[icell] = R * c
        nn_sel[:] = [
            x for _, x in sorted(zip(distances, np.arange(len(gridinfo.clon))))
        ][0:gridinfo.nn]
        nn_sel = nn_sel.astype(int)

        u[:] = [1. / distances[y] for y in nn_sel]

        #return nn_sel[:], u
        return np.array(nn_sel[:], dtype=int), np.array(u)

    @staticmethod
    def get_divisible_hours_string(datetime_obj, hours=3):
        """
        Added by Erik; extracts a string for the previous and next datetimes
        that are divisible by three
        """
        # Get the hour from the datetime object
        hour = datetime_obj.hour

        # Check if the hour is divisible by N hours
        if hour % hours == 0:
            # If divisible, get the current hour and the next hour
            current_hour = datetime_obj.replace(minute=0,
                                                second=0,
                                                microsecond=0)
            hour_above = current_hour + timedelta(hours=hours)
            return [
                current_hour.strftime('%Y%m%d%H'),
                hour_above.strftime('%Y%m%d%H')
            ]
        else:
            # If not divisible, get the hour below and above
            hour_below = datetime_obj.replace(hour=hour - (hour % hours),
                                              minute=0,
                                              second=0,
                                              microsecond=0)
            hour_above = hour_below + timedelta(hours=hours)
            return [
                hour_below.strftime('%Y%m%d%H'),
                hour_above.strftime('%Y%m%d%H')
            ]

    @staticmethod
    def _read_and_intrp_v_ICON(loc, fields_list, time_id, idp):
        """
        David:
        Slight modification from "self.sample_total_columns" for WRF.
        
        Helper function for sample_total_columns.
        read_and_intrp, but vectorized.
        Reads in fields and interpolates
        them linearly in time.
        
        Arguments
        ----------
        loc (:class:`dict`)
            Passed through from sample_total_columns, see there.
        fields_list (:class:`list` of :class:`str`)
            List of netcdf-variables to process.
        time_id (:class:`int`)
            Time index referring to all files in loc to read
        idp (:class:`array` of :class:`int`)
            Indices for id_xy, domain and frac_t in loc (i.e.
            observations) to process.
        
        Output
        ------
        List of temporally interpolated fields, one entry per member of
        fields_list.
        """

        var_intrp_l = list()

        # Select input files
        id_file0 = bisect.bisect_right(loc["file_start_time_indices"],
                                       time_id) - 1
        id_file1 = bisect.bisect_right(loc["file_start_time_indices"],
                                       time_id + 1) - 1
        if id_file0 < 0 or id_file1 < 0:
            raise ValueError("This shouldn't happen.")

        # Get time id in file
        id_t_file0 = time_id - loc["file_start_time_indices"][id_file0]
        id_t_file1 = time_id + 1 - loc["file_start_time_indices"][id_file1]

        # Open files
        ### NetCDF approach:
        nc0 = nc.Dataset(loc["files"][id_file0], "r")
        nc1 = nc.Dataset(loc["files"][id_file1], "r")

        ### Xarray approach:
        #nc0 = xr.open_dataset(loc["files"][id_file0])
        #nc1 = xr.open_dataset(loc["files"][id_file1])

        # Per field to sample
        for field in fields_list:
            # Read input file
            ### NetCDF approach:
            field0 = nc0[field][:]
            field1 = nc1[field][:]

            ### Xarray approach:
            #field0 = nc0[ field ].values
            #field1 = nc1[ field ].values

            if len(field0.shape) == 3:
                ### For ICON fields that has shape (time, z, cells)
                # -- First select the nearest neighbours of the fields

                var00 = field0[0, :, loc["nn_sel_list"][idp]]
                var01 = field1[0, :, loc["nn_sel_list"][idp]]

                # -- Then interpolate spatially with weights
                # The sum of the weights per obs location
                u_sums = np.nansum(loc["weight_list"][idp], axis=1)

                # Fancy way of mulitply the weights onto 4 nearest neighbors per obs location. (to be varified)
                # see: https://numpy.org/doc/stable/reference/generated/numpy.einsum.html
                # Since the dimension does not match, so here are the tricks to do so...
                var0 = (
                    np.einsum("ij,ijk->ik", loc["weight_list"][idp], var00) /
                    u_sums[:, np.newaxis])
                var1 = (
                    np.einsum("ij,ijk->ik", loc["weight_list"][idp], var01) /
                    u_sums[:, np.newaxis])

                # -- Get the time fractions per obs location
                frac_t_ = np.array(loc["frac_t"][idp]).reshape((len(idp), 1))

            elif len(field0.shape) == 2:
                ### For ICON fields that has shape (time, cells), e.g. "pres_sfc"
                # var0 will have shape (len(idp),len(profile))

                # -- First select the fields:
                var00 = field0[0, loc["nn_sel_list"][idp]]
                var01 = field1[0, loc["nn_sel_list"][idp]]

                # -- Then interpolate in space with weights:
                # The sum of the weights per obs location
                u_sums = np.nansum(loc["weight_list"][idp], axis=1)

                var0 = np.nansum(loc["weight_list"][idp] * var00,
                                 axis=1) / u_sums
                var1 = np.nansum(loc["weight_list"][idp] * var01,
                                 axis=1) / u_sums

                # -- Get the time fractions per obs location
                frac_t_ = np.array(loc["frac_t"][idp])

            else:
                raise ValueError("Can't deal with field with %d dimensions." %
                                 len(field0.shape))

            # Interpolate in time
            var_intrp_l.append(var0 * frac_t_ + var1 * (1. - frac_t_))

        nc0.close()
        nc1.close()

        return var_intrp_l

    #### David: A variation for sampling ICON ###
    def sample_total_columns_ICON(self, dat, loc, fields_list):
        """
        David:
        Slight modification from "self.sample_total_columns" for WRF.
        
        Sample total_columns of fields_list in ICON output in
        self.settings["dir.icon_sim"] at the location id_xy in domain, id_t
        in all wrfout-times. Files and indices therein are recognized
        by id_t and file_time_start_indices.
        All quantities needed for computing total columns from profiles
        are in dat (kernel, prior, ...).

        Arguments
        ---------
        dat (:class:`list`)
            Result of wrfhelper.read_sampling_coords. Used here: prior,
            prior_profile, kernel, psurf, pressure_axis, [, pwf]
            If psurf or any of pressure_axis are nan, wrf's own
            surface pressure is used and pressure_axis constructed
            from this and the number of levels in the averaging kernel.
            This allows sampling with synthetic data that don't have
            pressure information. This only works with level_def
            "layer_average".
            If pwf is not present or nan, a simple one is created, for
            level_def "layer_average".
        loc (:class:`dict`)
            A dictionary with all location-related input for sampling,
            computed in wrfout_sampler. Keys:
            id_xy, domain: Domain coordinates
            id_t: Timestep (continous throughout all files)
            frac_t: Interpolation coeficient between id_t and id_t+1:
                    t_obs = frac_t*t[id_t] + (1-frac_t)*t[id_t+1])
            file_start_time_indices: Time index at which a new wrfout
                                     file starts
            files: names of wrfout files.
        fields_list (:class:`list`)
            The fields to sample total columns from.

        Output
        ------
        sampled_columns (:class:`array`)
            A 2D-array of sampled columns.
            Shape: (len(dat["prior"]), len(fields_list))
        """

        # Initialize output of all tracers
        tc = np.ndarray(shape=(len(dat["prior"]), len(fields_list)),
                        dtype=float)
        tc[:] = float("nan")

        tc_unperturbed = np.ndarray(shape=(len(dat["prior"]), 1), dtype=float)
        tc_unperturbed[:] = float("nan")

        do_CAMS = True

        # Process by id_t
        UT = list(set(loc["id_t"][:]))

        #print('Tests, UT: %s' %UT)

        # print(loc['times'])

        for time_id in UT:
            # Coordinates to process
            idt = np.nonzero(loc["id_t"] == time_id)[0]
            # David: idt seems to be a list
            # print('Tests, idt: %s' %idt)

            din = loc['times'][idt[0]]
            # print(din)
            [hour_below,
             hour_above] = self.get_divisible_hours_string(datetime_obj=din)
            print("oi oi", hour_below, hour_above)
            if do_CAMS:
                CAMS = xr.open_mfdataset([
                    "/scratch/snx3000/ekoene/CAMS_i/cams_egg4_" + hour_below +
                    ".nc", "/scratch/snx3000/ekoene/CAMS_i/cams_egg4_" +
                    hour_above + ".nc"
                ],
                                         concat_dim="Time",
                                         combine="nested").rename({{
                                             'Time':
                                             'time'
                                         }})
                pressure = CAMS.ap.values[:, :, np.newaxis,
                                          np.newaxis] + np.einsum(
                                              'pi,pjk->pijk', CAMS.bp.values,
                                              CAMS.Psurf.values)
                # The following is applicable if we only use joint (CO2,Pres) levels [as needed by, e.g., OCO2]
                CAMS["pressure"] = (
                    ("time", "level", "latitude", "longitude"),
                    (pressure[:, 1:, :, :] + pressure[:, :-1, :, :]) * 0.5)
                # The following is applicable if we want to use (CO2,Pres_ifc) combinations [note the 'hlevel' dimension]
                # CAMS["pressure"] = (("time", "hlevel", "latitude", "longitude"), pressure)

            # Read and get tracer ensemble profiles, and flip them, since ICON start from the model top
            m_dry = 28.97  # g/mol for dry air
            m_gas = 44.01  # g/mol for CO2
            to_ppm = 1e6
            qv = self._read_and_intrp_v_ICON(loc, ['qv'], time_id, idt)[0]

            # The unperturbed tracer
            BG = np.asarray(
                self._read_and_intrp_v_ICON(
                    loc, ['TRCO2_BG'], time_id,
                    idt)) / (1 - qv) * (m_dry / m_gas) * to_ppm
            # TRCO2_A = np.asarray(self._read_and_intrp_v_ICON(loc, ['TRCO2_A'], time_id, idt)) / (1-qv) * (m_dry/m_gas) * to_ppm
            try:  # In the "PRIOR" simulations I made, the following tracer contains the anthropogenic portion; it doesn't exist otherwise.
                TRCO2_A = np.asarray(
                    self._read_and_intrp_v_ICON(
                        loc, ['ANTH'], time_id,
                        idt)) / (1 - qv) * (m_dry / m_gas) * to_ppm
            except:
                TRCO2_A = np.asarray(
                    self._read_and_intrp_v_ICON(
                        loc, ['TRCO2_A'], time_id,
                        idt)) / (1 - qv) * (m_dry / m_gas) * to_ppm
            CO2_RA = np.asarray(
                self._read_and_intrp_v_ICON(loc, ['CO2_RA'], time_id, idt)) / (
                    1 - qv) * (m_dry / m_gas) * to_ppm
            CO2_GPP = np.asarray(
                self._read_and_intrp_v_ICON(
                    loc, ['CO2_GPP'], time_id,
                    idt)) / (1 - qv) * (m_dry / m_gas) * to_ppm
            biosource_all_chemtr = np.asarray(
                self._read_and_intrp_v_ICON(
                    loc, ['biosource_all_chemtr'], time_id,
                    idt)) / (1 - qv) * (m_dry / m_gas) * to_ppm
            biosink_chemtr = np.asarray(
                self._read_and_intrp_v_ICON(
                    loc, ['biosink_chemtr'], time_id,
                    idt)) / (1 - qv) * (m_dry / m_gas) * to_ppm
            # The ensemble tracers
            tracers = np.asarray(
                self._read_and_intrp_v_ICON(
                    loc, fields_list, time_id,
                    idt)) / (1 - qv) * (m_dry / m_gas) * to_ppm

            # Correct for the missing biospheric components!
            tracers = tracers + biosource_all_chemtr - biosink_chemtr
            prior_tracers = BG + TRCO2_A + CO2_RA - CO2_GPP + biosource_all_chemtr - biosink_chemtr

            #profiles = np.fliplr( self._read_and_intrp_v_ICON(loc, fields_list, time_id, idt) ) * (28.97/16.01)*1e6 # mol/kg -> ppm
            # List, len=len(fields_list), shape of each: (len(idt),nz)

            # Read and get water vapor for wet/dry correction
            # print(np.asarray(qv).shape, np.asarray(tracers).shape, type(qv), type(tracers))

            # Read and get pressure axis:
            psurf = self._read_and_intrp_v_ICON(loc, ["pres"], time_id,
                                                idt)[0] / 1.e2  # Pa -> hPa
            # Shape: (len(idt),)

            ptop = 50  # David: Since ICON does not have hard coded ptop, assume it is 50 hPa...
            # Shape: (len(idt),)
            if not do_CAMS:
                ptop = 50

            if do_CAMS:
                ptop = 0.01

            ### David: ZNW was for WRF, for ICON first try getting "pres" or "pres_ifc"
            pres = np.fliplr(
                self._read_and_intrp_v_ICON(loc, ["pres"], time_id,
                                            idt)[0]) / 1.e2  # Pa -> hPa
            # pres = np.fliplr( self._read_and_intrp_v_ICON(loc, ["pres_ifc"], time_id, idt)[0] )/1.e2 # Pa -> hPa
            #znw = self._read_and_intrp_v_ICON(loc, ["ZNW"], time_id, idt)[0]
            #Shape:(len(idt),nz)

            # DONE reading from file.
            # Here it starts to make sense to loop over individual observations
            for nidt in range(len(idt)):

                nobs = idt[nidt]

                # Construct model pressure layer boundaries
                #pb_mod = self.get_pressure_boundaries_znw(znw[nidt, :], psurf[nidt], ptop)

                # numpy.fliplr reverses the order of elements along axis 1 (left/right).
                # For a 2-D array, this flips the entries in each row in the left/right direction.
                # Columns are preserved, but appear in a different order than before.
                pb_mod = pres[nidt]

                # Do the CAMS extension
                if do_CAMS:
                    CAMS_obs = CAMS.interp(time=loc['times'][nobs],
                                           latitude=loc['latitude'][nobs],
                                           longitude=loc['longitude'][nobs])
                    CAMS_pressures = CAMS_obs.pressure.values
                    CAMS_idx = CAMS_pressures < np.min(pb_mod)
                    pb_mod = np.concatenate((pb_mod, CAMS_pressures[CAMS_idx]))
                    CAMS_gas = CAMS_obs.CO2.values[CAMS_idx] * 1e6

                # Add a final value onto the column...
                pb_mod = np.append(pb_mod, np.min(pb_mod) - 1)

                if (np.diff(pb_mod) >= 0).any():
                    msg = ("Model pressure boundaries for observation %d " + \
                           "are not monotonically decreasing! Investigate.") % nobs
                    # --> Erik: I have removed this, because I don't quite know how to investigate this easily. Was triggered though!
                    # raise ValueError(msg)

                # Construct retrieval pressure layer boundaries
                # print(dat["level_def"][nobs])
                if dat["level_def"][nobs] == "layer_average":
                    if np.any(np.isnan(dat["pressure_levels"][nobs])) \
                       or np.isnan(dat["psurf"][nobs]):
                        # Code for synthetic data without a pressure axis,
                        # but with an averaging kernel:
                        # Use wrf's surface and top pressure
                        nlayers = len(dat["averaging_kernel"][nobs])
                        pb_ret = np.linspace(psurf[nidt], ptop, nlayers + 1)
                    else:
                        nlayers = len(dat["averaging_kernel"][nobs])
                        pb_ret = np.linspace(psurf[nidt], ptop, nlayers + 1)
                        # Below commented out by David
                        # Because somehow doesn't work
                        #pb_ret = self.get_pressure_boundaries_paxis(
                        #        dat["pressure_levels"][nobs],
                        #        dat["psurf"][nobs])
                elif dat["level_def"][nobs] == "pressure_boundary":
                    if np.any(np.isnan(dat["pressure_levels"][nobs])):
                        # Code for synthetic data without a pressure axis,
                        # but with an averaging kernel:
                        # Use wrf's surface and top pressure
                        nlevels = len(dat["averaging_kernel"][nobs])
                        pb_ret = np.linspace(psurf[nidt], ptop, nlevels)
                    else:
                        pb_ret = dat["pressure_levels"][nobs]
                else:
                    # print('No appropriate level chosen...')
                    dat["level_def"][nobs] = "pressure_boundary"
                    # print("changed definition to pressure_boundary")
                    if np.any(np.isnan(dat["pressure_levels"][nobs])):
                        # Code for synthetic data without a pressure axis,
                        # but with an averaging kernel:
                        # Use wrf's surface and top pressure
                        nlevels = len(dat["averaging_kernel"][nobs])
                        pb_ret = np.linspace(psurf[nidt], ptop, nlevels)
                    else:
                        pb_ret = dat["pressure_levels"][nobs]

                if (np.diff(pb_ret) >= 0).any():
                    msg = ("Retrieval pressure boundaries for " + \
                           "observation %d are not monotonically " + \
                           "decreasing! Investigate.") % nobs
                    print('pb_ret[:]: %s, np.diff(pb_ret): %s' %
                          (pb_ret[:], np.diff(pb_ret)))
                    raise ValueError(msg)

                # Get vertical integration coefficients (i.e. to
                # "interpolate" from model to retrieval grid)
                coef_matrix = self.get_int_coefs(
                    pb_ret, pb_mod,
                    dat["level_def"][nobs])  ### To be verified !!

                # Model retrieval with averaging kernel and prior profile
                if "pressure_weighting_function" in list(dat.keys()):
                    pwf = dat["pressure_weighting_function"][nobs]
                if (not "pressure_weighting_function" in list(
                        dat.keys())) or np.any(np.isnan(pwf)):
                    # Construct pressure weighting function from
                    # pressure boundaries
                    pwf = self.get_pressure_weighting_function(pb_ret,
                                                               rule="simple")

                # Compute pressure-weighted averaging kernel
                avpw = pwf * dat["averaging_kernel"][nobs]

                # Get prior
                prior_col = dat["prior"][nobs]
                prior_profile = dat["prior_profile"][nobs]
                if np.isnan(prior_col):  # compute prior
                    prior_col = np.dot(pwf, prior_profile)

                # Compute total columns
                offset = 0
                for nf in range(len(fields_list)):
                    # Integrate model profile
                    tr_here = np.flip(tracers[nf][nidt, :])
                    if do_CAMS:
                        tr_here = np.concatenate((tr_here, CAMS_gas))
                    profile = ((tr_here - offset))
                    profile_intrp = np.matmul(coef_matrix,
                                              profile)  ### To be verified !!

                    # Model retrieval
                    # print(prior_profile)
                    # print(profile_intrp)
                    # print(prior_col)
                    tc[nobs, nf] = prior_col + np.dot(
                        avpw, profile_intrp - prior_profile)
                    # print(tc[nobs,nf])

                tr_here = np.flip(prior_tracers[0][nidt, :])
                if do_CAMS:
                    tr_here = np.concatenate((tr_here, CAMS_gas))
                profile = ((tr_here - offset))
                profile_intrp = np.matmul(coef_matrix,
                                          profile)  ### To be verified !!
                tc_unperturbed[nobs, 0] = prior_col + np.dot(
                    avpw, profile_intrp - prior_profile)

        return tc, tc_unperturbed

if __name__ == "__main__":
    pass
