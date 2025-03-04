"""CarbonTracker Data Assimilation Shell (CTDAS) Copyright (C) 2017 Wouter Peters. 
Users are recommended to contact the developers (wouter.peters@wur.nl) to receive
updates of the code. See also: http://www.carbontracker.eu. 

This program is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software Foundation, 
version 3. This program is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 

You should have received a copy of the GNU General Public License along with this 
program. If not, see <http://www.gnu.org/licenses/>."""
#!/usr/bin/env python
# optimizer.py
"""
.. module:: optimizer
.. moduleauthor:: Wouter Peters 

Revision History:
File created on 28 Jul 2010.

"""

import logging
import numpy as np
import numpy.linalg as la
import da.tools.io4 as io
import csv
import xarray as xr
from sklearn.metrics.pairwise import haversine_distances

identifier = 'Optimizer baseclass'
version = '0.0'

################### Begin Class Optimizer ###################


class Optimizer(object):
    """
        This creates an instance of an optimization object. It handles the minimum least squares optimization
        of the state vector given a set of sample objects. Two routines will be implemented: one where the optimization
        is sequential and one where it is the equivalent matrix solution. The choice can be made based on considerations of speed
        and efficiency.
    """

    def __init__(self):
        self.ID = identifier
        self.version = version

        logging.info('Optimizer object initialized: %s' % self.ID)

    def setup(self, dims, loc_coeff_file):
        self.nlag = dims[0]
        self.nmembers = dims[1]
        self.nparams = dims[2]
        self.nobs = dims[3]
        self.loc_coeffs = loc_coeff_file
        self.create_matrices()

    def create_matrices(self):
        """ Create Matrix space needed in optimization routine """

        # mean state  [X]
        self.x = np.zeros((self.nlag * self.nparams, ), float)
        # deviations from mean state  [X']
        self.X_prime = np.zeros((
            self.nlag * self.nparams,
            self.nmembers,
        ), float)
        # mean state, transported to observation space [ H(X) ]
        self.Hx = np.zeros((self.nobs, ), float)
        # deviations from mean state, transported to observation space [ H(X') ]
        self.HX_prime = np.zeros((self.nobs, self.nmembers), float)
        # observations
        self.obs = np.zeros((self.nobs, ), float)
        # observation ids
        self.obs_ids = np.zeros((self.nobs, ), float)
        # covariance of observations
        # Total covariance of fluxes and obs in units of obs [H P H^t + R]
        if self.algorithm == 'Serial':
            self.R = np.zeros((self.nobs, ), float)
            self.HPHR = np.zeros((self.nobs, ), float)
        else:
            self.R = np.zeros((
                self.nobs,
                self.nobs,
            ), float)
            self.HPHR = np.zeros((
                self.nobs,
                self.nobs,
            ), float)
        # localization of obs
        self.may_localize = np.zeros(self.nobs, bool)
        # rejection of obs
        self.may_reject = np.zeros(self.nobs, bool)
        # flags of obs
        self.flags = np.zeros(self.nobs, int)
        # species type
        self.species = np.zeros(self.nobs, str)
        # species type
        self.sitecode = np.zeros(self.nobs, str)
        # rejection_threshold
        self.rejection_threshold = np.zeros(self.nobs, float)
        # lat/lon
        self.latitude = np.zeros(self.nobs, float)
        self.longitude = np.zeros(self.nobs, float)

        # species mask
        self.speciesmask = {{}}

        # Kalman Gain matrix
        #self.KG = np.zeros((self.nlag * self.nparams, self.nobs,), float)
        self.KG = np.zeros((self.nlag * self.nparams, ), float)

        #msteiner:
        self.evn = np.zeros(self.nobs, str)
        self.fromfile = np.zeros(self.nobs, str)

        #read loc_coeffs from file
        ds = xr.open_dataset(self.loc_coeffs)
        self.coeff_matrix = np.exp(
            -ds.Distances.values / 400
        ).T  # ERIK: I set this to 400 as a rough footprint size for a station (was 600 km for Michael; 60 km for Nikolai)
        self.name_array = ds.Stations_names.values

    def state_to_matrix(self, statevector):
        allsites = []  # collect all obs for n=1,..,nlag
        allobs = []  # collect all obs for n=1,..,nlag
        allmdm = []  # collect all mdm for n=1,..,nlag
        allids = []  # collect all model samples for n=1,..,nlag
        allreject = []  # collect all model samples for n=1,..,nlag
        alllocalize = []  # collect all model samples for n=1,..,nlag
        allflags = []  # collect all model samples for n=1,..,nlag
        allspecies = []  # collect all model samples for n=1,..,nlag
        allsimulated = []  # collect all members model samples for n=1,..,nlag
        allrej_thres = [
        ]  # collect all rejection_thresholds, will be the same for all samples of same source
        alllats = []  # collect all latitudes for n=1,..,nlag
        alllons = []  # collect all longitudes for n=1,..,nlag
        #msteiner:
        allevns = []  # collect all evns for finding loc_coeffs in localize()
        allfromfiles = [
        ]  # collect all evns for finding loc_coeffs in localize()

        for n in range(self.nlag):
            samples = statevector.obs_to_assimilate[n]
            members = statevector.ensemble_members[n]
            self.x[n * self.nparams:(n + 1) *
                   self.nparams] = members[0].param_values
            self.X_prime[n * self.nparams:(n + 1) *
                         self.nparams, :] = np.transpose(
                             np.array([m.param_values for m in members]))

            # Add observation data for all sample objects
            if samples != None:
                if type(samples) != list: samples = [samples]
                for m in range(len(samples)):
                    sample = samples[m]
                    logging.debug(
                        'Lag %i, sample %i: rejection_threshold = %i, nobs = %i'
                        %
                        (n, m, sample.rejection_threshold, sample.getlength()))
                    logging.info(
                        'Lag %i, sample %i: rejection_threshold = %i, nobs = %i'
                        %
                        (n, m, sample.rejection_threshold, sample.getlength()))
                    logging.info(f'{{dir(sample)}}')
                    alllats.extend(sample.getvalues('lat'))
                    alllons.extend(sample.getvalues('lon'))
                    allrej_thres.extend([sample.rejection_threshold] *
                                        sample.getlength())
                    allreject.extend(sample.getvalues('may_reject'))
                    alllocalize.extend(sample.getvalues('may_localize'))
                    allflags.extend(sample.getvalues('flag'))
                    allspecies.extend(sample.getvalues('species'))
                    allobs.extend(sample.getvalues('obs'))
                    allsites.extend(sample.getvalues('code'))
                    allmdm.extend(sample.getvalues('mdm'))
                    allids.extend(sample.getvalues('id'))
                    #msteiner:
                    # if sample.get_samples_type() == 'insitu':
                    try:
                        allevns.extend(sample.getvalues('evn'))
                        allfromfiles.extend(sample.getvalues('fromfile'))
                    except:
                        logging.debug(
                            f"Number of copies: {{len(sample.getvalues('lat'))}}"
                        )
                        allevns.extend(['column'] *
                                       len(sample.getvalues('lat')))
                        allfromfiles.extend(['column'] *
                                            len(sample.getvalues('lat')))
                    simulatedensemble = sample.getvalues('simulated')
                    for s in range(simulatedensemble.shape[0]):
                        allsimulated.append(simulatedensemble[s])

        self.latitude[:] = np.array(alllats)
        self.longitude[:] = np.array(alllons)
        self.rejection_threshold[:] = np.array(allrej_thres)
        self.obs[:] = np.array(allobs)
        self.obs_ids[:] = np.array(allids)
        self.HX_prime[:, :] = np.array(allsimulated)
        self.Hx[:] = self.HX_prime[:, 0]

        self.may_reject[:] = np.array(allreject)
        self.may_localize[:] = np.array(alllocalize)
        self.flags[:] = np.array(allflags)
        self.species[:] = np.array(allspecies)
        self.sitecode = allsites

        #msteiner:
        # self.evn = allevns
        self.fromfile = allfromfiles

        # ~~~~~~~~ NEW SINCE OCO2, but generally valid: Setup localization (distance between observations and regions)
        OBSERVATIONS_IN_RADIANS_LATLON = np.deg2rad(
            np.column_stack([self.latitude, self.longitude]))
        grid = xr.open_dataset(
            '/users/ekoene/CTDAS_inputs/icon_europe_DOM01.nc')
        grid_latitudes = grid.lat_cell_centre.values
        grid_longitudes = grid.lon_cell_centre.values
        REGIONS_IN_RADIANS_LATLON = np.column_stack(
            [grid_latitudes, grid_longitudes])
        Distances = haversine_distances(
            OBSERVATIONS_IN_RADIANS_LATLON,
            REGIONS_IN_RADIANS_LATLON) * 6371000 / 1000  # distance to km s
        logging.debug(Distances)
        self.coeff_matrix = np.exp(
            -Distances / 400
        )  # ERIK: I set this to 400 as a rough footprint size for a station (was 600 km for Michael; 60 km for Nikolai)
        self.name_array = np.arange(
            OBSERVATIONS_IN_RADIANS_LATLON.shape[0]
        )  # These should be 'names' but my pixels don't have names, of course!

        self.X_prime = self.X_prime - self.x[:, np.
                                             newaxis]  # make into a deviation matrix
        self.HX_prime = self.HX_prime - self.Hx[:, np.
                                                newaxis]  # make a deviation matrix

        if self.algorithm == 'Serial':
            for i, mdm in enumerate(allmdm):
                self.R[i] = mdm**2
        else:
            for i, mdm in enumerate(allmdm):
                self.R[i, i] = mdm**2

    def matrix_to_state(self, statevector):
        for n in range(self.nlag):
            members = statevector.ensemble_members[n]
            for m, mem in enumerate(members):
                members[m].param_values[:] = self.X_prime[
                    n * self.nparams:(n + 1) * self.nparams,
                    m] + self.x[n * self.nparams:(n + 1) * self.nparams]

        #msteiner:
        statevector.isOptimized = True
        #---------

        logging.debug(
            'Returning optimized data to the StateVector, setting "StateVector.isOptimized = True" '
        )

    def write_diagnostics(self, filename, type):
        """
            Open a NetCDF file and write diagnostic output from optimization process:

                - calculated residuals
                - model-data mismatches
                - HPH^T
                - prior ensemble of samples
                - posterior ensemble of samples
                - prior ensemble of fluxes
                - posterior ensemble of fluxes

            The type designation refers to the writing of prior or posterior data and is used in naming the variables"
        """

        # Open or create file

        if type == 'prior':
            f = io.CT_CDF(filename, method='create')
            logging.debug('Creating new diagnostics file for optimizer (%s)' %
                          filename)
        elif type == 'optimized':
            f = io.CT_CDF(filename, method='write')
            logging.debug(
                'Opening existing diagnostics file for optimizer (%s)' %
                filename)

        # Add dimensions

        dimparams = f.add_params_dim(self.nparams)
        dimmembers = f.add_members_dim(self.nmembers)
        dimlag = f.add_lag_dim(self.nlag, unlimited=False)
        dimobs = f.add_obs_dim(self.nobs)
        dimstate = f.add_dim('nstate', self.nparams * self.nlag)
        dim200char = f.add_dim('string_of200chars', 200)

        # Add data, first the ones that are written both before and after the optimization

        savedict = io.std_savedict.copy()
        savedict['name'] = "statevectormean_%s" % type
        savedict['long_name'] = "full_statevector_mean_%s" % type
        savedict['units'] = "unitless"
        savedict['dims'] = dimstate
        savedict['values'] = self.x.tolist()
        savedict['comment'] = 'Full %s state vector mean ' % type
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "statevectordeviations_%s" % type
        savedict['long_name'] = "full_statevector_deviations_%s" % type
        savedict['units'] = "unitless"
        savedict['dims'] = dimstate + dimmembers
        savedict['values'] = self.X_prime.tolist()
        savedict[
            'comment'] = 'Full state vector %s deviations as resulting from the optimizer' % type
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "modelsamplesmean_%s" % type
        savedict['long_name'] = "modelsamplesforecastmean_%s" % type
        savedict['units'] = "mol mol-1"
        savedict['dims'] = dimobs
        savedict['values'] = self.Hx.tolist()
        savedict[
            'comment'] = '%s mean mole fractions based on %s state vector' % (
                type, type)
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "modelsamplesdeviations_%s" % type
        savedict['long_name'] = "modelsamplesforecastdeviations_%s" % type
        savedict['units'] = "mol mol-1"
        savedict['dims'] = dimobs + dimmembers
        savedict['values'] = self.HX_prime.tolist()
        savedict[
            'comment'] = '%s mole fraction deviations based on %s state vector' % (
                type, type)
        f.add_data(savedict)

        # Continue with prior only data

        if type == 'prior':

            savedict = io.std_savedict.copy()
            savedict['name'] = "sitecode"
            savedict[
                'long_name'] = "site code propagated from observation file"
            savedict['dtype'] = "char"
            savedict['dims'] = dimobs + dim200char
            savedict['values'] = self.sitecode
            savedict['missing_value'] = '!'
            f.add_data(savedict)

            savedict = io.std_savedict.copy()
            savedict['name'] = "observed"
            savedict['long_name'] = "observedvalues"
            savedict['units'] = "mol mol-1"
            savedict['dims'] = dimobs
            savedict['values'] = self.obs.tolist()
            savedict['comment'] = 'Observations used in optimization'
            f.add_data(savedict)

            savedict = io.std_savedict.copy()
            savedict['name'] = "obspack_num"
            savedict['dtype'] = "int64"
            savedict['long_name'] = "Unique_ObsPack_observation_number"
            savedict['units'] = ""
            savedict['dims'] = dimobs
            savedict['values'] = self.obs_ids.tolist()
            savedict[
                'comment'] = 'Unique observation number across the entire ObsPack distribution'
            f.add_data(savedict)

            savedict = io.std_savedict.copy()
            savedict['name'] = "modeldatamismatchvariance"
            savedict['long_name'] = "modeldatamismatch variance"
            savedict['units'] = "[mol mol-1]^2"
            if self.algorithm == 'Serial':
                savedict['dims'] = dimobs
            else:
                savedict['dims'] = dimobs + dimobs
            savedict['values'] = self.R.tolist()
            savedict[
                'comment'] = 'Variance of mole fractions resulting from model-data mismatch'
            f.add_data(savedict)

        # Continue with posterior only data

        elif type == 'optimized':

            savedict = io.std_savedict.copy()
            savedict['name'] = "totalmolefractionvariance"
            savedict['long_name'] = "totalmolefractionvariance"
            savedict['units'] = "[mol mol-1]^2"
            if self.algorithm == 'Serial':
                savedict['dims'] = dimobs
            else:
                savedict['dims'] = dimobs + dimobs
            savedict['values'] = self.HPHR.tolist()
            savedict[
                'comment'] = 'Variance of mole fractions resulting from prior state and model-data mismatch'
            f.add_data(savedict)

            savedict = io.std_savedict.copy()
            savedict['name'] = "flag"
            savedict['long_name'] = "flag_for_obs_model"
            savedict['units'] = "None"
            savedict['dims'] = dimobs
            savedict['values'] = self.flags.tolist()
            savedict[
                'comment'] = 'Flag (0/1/2/99) for observation value, 0 means okay, 1 means QC error, 2 means rejected, 99 means not sampled'
            f.add_data(savedict)

            #savedict = io.std_savedict.copy()
            #savedict['name'] = "kalmangainmatrix"
            #savedict['long_name'] = "kalmangainmatrix"
            #savedict['units'] = "unitless molefraction-1"
            #savedict['dims'] = dimstate + dimobs
            #savedict['values'] = self.KG.tolist()
            #savedict['comment'] = 'Kalman gain matrix of all obs and state vector elements'
            #dummy                   = f.add_data(savedict)

        f.close()
        logging.debug('Diagnostics file closed')

    def serial_minimum_least_squares(self, n_bg_params=0):
        """ Make minimum least squares solution by looping over obs"""

        # Calculate prior value cost function (observation part)
        res_prior = np.abs(self.obs - self.Hx)
        select = (res_prior < 1E15).nonzero()[0]
        J_prior = res_prior.take(select, axis=0)**2 / self.R.take(select,
                                                                  axis=0)
        res_prior = np.mean(res_prior)
        for n in range(self.nobs):

            # Screen for flagged observations (for instance site not found, or no sample written from model)

            if self.flags[n] != 0:
                logging.debug(
                    'Skipping observation (%s,%i) because of flag value %d' %
                    (self.sitecode[n], self.obs_ids[n], self.flags[n]))
                logging.info(
                    'Skipping observation (%s,%i) because of flag value %d' %
                    (self.sitecode[n], self.obs_ids[n], self.flags[n]))
                continue

            # Screen for outliers greather than 3x model-data mismatch, only apply if obs may be rejected

            res = self.obs[n] - self.Hx[n]

            if self.may_reject[n]:
                threshold = self.rejection_threshold[n] * np.sqrt(self.R[n])
                if np.abs(res) > threshold:
                    logging.debug(
                        'Rejecting observation (%s,%i) because residual (%f) exceeds threshold (%f)'
                        % (self.sitecode[n], self.obs_ids[n], res, threshold))
                    logging.info(
                        'Rejecting observation (%s,%i) because residual (%f) exceeds threshold (%f)'
                        % (self.sitecode[n], self.obs_ids[n], res, threshold))
                    self.flags[n] = 2
                    continue

            logging.debug('Proceeding to assimilate observation %s, %i' %
                          (self.sitecode[n], self.obs_ids[n]))
            logging.info('Proceeding to assimilate observation %s, %i' %
                         (self.sitecode[n], self.obs_ids[n]))

            PHt = 1. / (self.nmembers - 1) * np.dot(self.X_prime,
                                                    self.HX_prime[n, :])
            self.HPHR[n] = 1. / (self.nmembers - 1) * (
                self.HX_prime[n, :] * self.HX_prime[n, :]).sum() + self.R[n]
            self.KG[:] = PHt / self.HPHR[n]

            if self.may_localize[n]:
                logging.debug('Trying to localize observation %s, %i' %
                              (self.sitecode[n], self.obs_ids[n]))
                logging.info('Trying to localize observation %s, %i' %
                             (self.sitecode[n], self.obs_ids[n]))
                self.localize(n, n_bg_params)
            else:
                logging.debug('Not allowed to localize observation %s, %i' %
                              (self.sitecode[n], self.obs_ids[n]))
#                logging.info('Not allowed to localize observation %s, %i' % (self.sitecode[n], self.obs_ids[n]))

            alpha = np.double(1.0) / (np.double(1.0) + np.sqrt(
                (self.R[n]) / self.HPHR[n]))

            self.x[:] = self.x + self.KG[:] * res

            for r in range(self.nmembers):
                #                logging.info('X_prime before: %s'%(str(self.X_prime[:, r])))
                self.X_prime[:,
                             r] = self.X_prime[:, r] - alpha * self.KG[:] * (
                                 self.HX_prime[n, r])
#                logging.info('X_prime after: %s'%(str(self.X_prime[:, r])))
#                logging.info('======================================')
            del r

            # update samples to account for update of statevector based on observation n
            HXprime_n = self.HX_prime[n, :].copy()
            res = self.obs[n] - self.Hx[n]
            fac = 1.0 / (self.nmembers - 1) * np.sum(
                HXprime_n[np.newaxis, :] * self.HX_prime,
                axis=1) / self.HPHR[n]
            self.Hx = self.Hx + fac * res
            self.HX_prime = self.HX_prime - alpha * fac[:,
                                                        np.newaxis] * HXprime_n

        del n
        if 'HXprime_n' in globals(): del HXprime_n

        # calculate posterior value cost function
        res_post = np.abs(self.obs - self.Hx)
        select = (res_post < 1E15).nonzero()[0]
        J_post = res_post.take(select, axis=0)**2 / self.R.take(select, axis=0)
        res_post = np.mean(res_post)

        logging.info(
            'Observation part cost function: prior = %s, posterior = %s' %
            (np.mean(J_prior), np.mean(J_post)))
        logging.info('Mean residual: prior = %s, posterior = %s' %
                     (res_prior, res_post))

#WP !!!! Very important to first do all obervations from n=1 through the end, and only then update 1,...,n. The current observation
#WP      should always be updated last because it features in the loop of the adjustments !!!!
#
#            for m in range(n + 1, self.nobs):
#                res = self.obs[n] - self.Hx[n]
#                fac = 1.0 / (self.nmembers - 1) * (self.HX_prime[n, :] * self.HX_prime[m, :]).sum() / self.HPHR[n]
#                self.Hx[m] = self.Hx[m] + fac * res
#                self.HX_prime[m, :] = self.HX_prime[m, :] - alpha * fac * self.HX_prime[n, :]
#
#            for m in range(1, n + 1):
#                res = self.obs[n] - self.Hx[n]
#                fac = 1.0 / (self.nmembers - 1) * (self.HX_prime[n, :] * self.HX_prime[m, :]).sum() / self.HPHR[n]
#                self.Hx[m] = self.Hx[m] + fac * res
#                self.HX_prime[m, :] = self.HX_prime[m, :] - alpha * fac * self.HX_prime[n, :]

    def bulk_minimum_least_squares(self):
        """ Make minimum least squares solution by solving matrix equations"""

        # Create full solution, first calculate the mean of the posterior analysis

        HPH = np.dot(self.HX_prime, np.transpose(self.HX_prime)) / (
            self.nmembers - 1)  # HPH = 1/N * HX' * (HX')^T
        self.HPHR[:, :] = HPH + self.R  # HPHR = HPH + R
        HPb = np.dot(self.X_prime, np.transpose(self.HX_prime)) / (
            self.nmembers - 1)  # HP = 1/N X' * (HX')^T
        self.KG[:, :] = np.dot(HPb, la.inv(self.HPHR))  # K = HP/(HPH+R)

        for n in range(self.nobs):
            self.localize(n)

        self.x[:] = self.x + np.dot(self.KG,
                                    self.obs - self.Hx)  # xa = xp + K (y-Hx)

        # And next make the updated ensemble deviations. Note that we calculate P by using the full equation (10) at once, and
        # not in a serial update fashion as described in Whitaker and Hamill.
        # For the current problem with limited N_obs this is easier, or at least more straightforward to do.

        I = np.identity(self.nlag * self.nparams)
        sHPHR = la.cholesky(self.HPHR)  # square root of HPH+R
        part1 = np.dot(HPb, np.transpose(la.inv(sHPHR)))  # HP(sqrt(HPH+R))^-1
        part2 = la.inv(sHPHR + np.sqrt(self.R))  # (sqrt(HPH+R)+sqrt(R))^-1
        Kw = np.dot(part1, part2)  # K~
        self.X_prime[:, :] = np.dot(I, self.X_prime) - np.dot(
            Kw, self.HX_prime)  # HX' = I - K~ * HX'

        # Now do the adjustments of the modeled mole fractions using the linearized ensemble. These are not strictly needed but can be used
        # for diagnosis.

        part3 = np.dot(HPH, np.transpose(la.inv(sHPHR)))  # HPH(sqrt(HPH+R))^-1
        Kw = np.dot(part3, part2)  # K~
        self.Hx[:] = self.Hx + np.dot(np.dot(HPH, la.inv(
            self.HPHR)), self.obs - self.Hx)  # Hx  = Hx+ HPH/HPH+R (y-Hx)
        self.HX_prime[:, :] = self.HX_prime - np.dot(
            Kw, self.HX_prime)  # HX' = HX'- K~ * HX'

        logging.info(
            'Minimum Least Squares solution was calculated, returning')

    def set_localization(self, loctype='None'):
        """ determine which localization to use """

        if loctype == 'CT2007':
            self.localization = True
            self.localizetype = 'CT2007'
            #T-test values for two-tailed student's T-test using 95% confidence interval for some options of nmembers
            if self.nmembers == 50:
                self.tvalue = 2.0086
            elif self.nmembers == 100:
                self.tvalue = 1.9840
            elif self.nmembers == 150:
                self.tvalue = 1.97591
            elif self.nmembers == 192:
                self.tvalue = 1.9724
            elif self.nmembers == 200:
                self.tvalue = 1.9719
            else:
                self.tvalue = 0
        elif loctype == 'spatial':
            logging.info('Spatial localization selected')
            self.localization = True
            self.localizetype = 'spatial'
        else:
            self.localization = False
            self.localizetype = 'None'

        logging.info("Current localization option is set to %s" %
                     self.localizetype)
        if ((self.localization == True) and (self.localizetype == 'CT2007')):
            if self.tvalue == 0:
                logging.error(
                    "Critical tvalue for localization not set for %i ensemble members"
                    % (self.nmembers))
                sys.exit(2)
            else:
                logging.info(
                    "Used critical tvalue %0.05f is based on 95%% probability and %i ensemble members in a two-tailed student's T-test"
                    % (self.tvalue, self.nmembers))

    def get_prob(self, n, i):
        #    def get_prob(self,obsdev,paramdev,r):
        """Calculate probability from correlations"""
        #        corr = np.corrcoef(self.HX_prime[n, :], self.X_prime[r, :].squeeze())[0, 1]
        #        corr = np.corrcoef(obsdev,paramdev)[0,1]
        #        corr = np.ma.corrcoef(np.ma.masked_invalid(self.HX_prime[n, :]),np.ma.masked_invalid(self.X_prime[r, :].squeeze()))[0,1]
        for r in np.arange(i, self.nlag * self.nparams)[::36]:
            corr = np.corrcoef(self.HX_prime[n, :],
                               self.X_prime[r, :].squeeze())[0, 1]
            prob = corr / np.sqrt(
                (1.000000001 - corr**2) / (self.nmembers - 2))
            if abs(prob) < self.tvalue:
                self.KG[r] = 0.0

    def localize(self, n, n_bg_params):
        skip_stations = [
            'Malin Head_47',
            'Hegyhatsal hatterszennyettseg-mero allomas_48',
            'Hegyhatsal hatterszennyettseg-mero allomas_82',
            'Birkenes_2',
            'Hegyhatsal hatterszennyettseg-mero allomas_115',
            'Hegyhatsal hatterszennyettseg-mero allomas_10',
            'Beromunster_12',
            'Beromunster_44',
            'Beromunster_72',
            'Beromunster_132',
            'Bilsdale_42',
            'Bilsdale_108',
            'Cabauw_27',
            'Cabauw_67',
            'Cabauw_127',
            'Gartow_30',
            'Gartow_60',
            'Gartow_132',
            'Gartow_216',
            'Hohenpeissenberg_50',
            'Hohenpeissenberg_93',
            'Hyltemossa_30',
            'Hyltemossa_70',
            'Ispara_40',
            'Ispra_70',
            'Karlsruhe_30',
            'Karlsruhe_60',
            'Karlsruhe_100',
            'Kresin u Pacova_10',
            'Kresin u Pacova_50',
            'Kresin u Pacova_125',
            'Lindenberg_2',
            'Lindenberg_10',
            'Lindenberg_40',
            'Observatoire de Haute Provence_10',
            'Observatoire de Haute Provence_50',
            "Observatoire perenne de l'environnement_10",
            "Observatoire perenne de l'environnement_50",
            'Ridge Hill_45',
            'Saclay_15',
            'Saclay_60',
            'Tacolneston_54',
            'Tacolneston_100',
            'Torfhaus_10',
            'Torfhaus_76',
            'Torfhaus_110',
            'Trainou_5',
            'Trainou_50',
            'Trainou_100',
        ]
        """ localize the Kalman Gain matrix """
        import numpy as np
        from multiprocessing import Pool

        if not self.localization:
            logging.debug('Not localized observation %i' % self.obs_ids[n])
            return
        if self.localizetype == 'CT2007':

            #            count_localized = 0
            #            for r in range(self.nlag * self.nparams):
            ##                corr = np.corrcoef(self.HX_prime[n, :], self.X_prime[r, :].squeeze())[0, 1]
            #                corr = np.ma.corrcoef(np.ma.masked_invalid(self.HX_prime[n, :]),np.ma.masked_invalid(self.X_prime[r, :].squeeze()))[0,1]
            #                prob = corr / np.sqrt((1.000000001 - corr ** 2) / (self.nmembers - 2))
            #                if abs(prob) < self.tvalue:
            #                    self.KG[r] = 0.0
            #                    count_localized = count_localized + 1
            #            logging.debug('Localized observation %i, %i%% of values set to 0' % (self.obs_ids[n],count_localized*100/(self.nlag * self.nparams)))
            #            logging.info('Localized observation %i, %i%% of values set to 0' % (self.obs_ids[n],count_localized*100/(self.nlag * self.nparams)))

            ############################################
            ###make the CT2007 parallel:
            #            args = [ (n, i) for i in range(self.nlag * self.nparams) ]
            args = [(n, i) for i in range(36)]
            #            args = [ (self.HX_prime[n, :], self.X_prime[r, :].squeeze(), r ) for r in range(self.nlag * self.nparams) ]
            with Pool(36) as pool:
                pool.starmap(self.get_prob, args)
#            count_localized = 0
#            for r in range(self.nlag * self.nparams):
#                if abs(prob[r]) < self.tvalue:
#                    self.KG[r] = 0.0
#                    count_localized = count_localized + 1
#            logging.debug('Localized observation %i, %i%% of values set to 0' % (self.obs_ids[n],count_localized*100/(self.nlag * self.nparams)))
#            logging.info('Localized observation %i, %i%% of values set to 0' % (self.obs_ids[n],count_localized*100/(self.nlag * self.nparams)))
            logging.info('Localized observation %i' % (self.obs_ids[n]))
            ############################################

        elif self.localizetype == 'spatial':
            # ###            if self.loc_L[n] > 0:
            # ###                obslati, obsloni = self.find_coord_index(self.latitude[n],self.longitude[n],180,360)
            # ###                for l in range(self.nlag):
            # ###                    self.KG[l*self.nparams:(l+1)*self.nparams] = np.multiply(self.KG[l*self.nparams:(l+1)*self.nparams], self.loc_coeff[str(self.loc_L[n])][obslati,obsloni,:])
            # ###                logging.debug('Localized observation %i with localization length %s' %(self.obs_ids[n], self.loc_L[n]))
            #             print(self.latitude[n], self.longitude[n], "lat and lon!")

            #             n_em_cat = 2
            #             lfound = False
            #             for iname,stationname in enumerate(self.name_array):
            #                 if stationname in skip_stations: continue # Skip stations outside of the domain!
            #                 if stationname==self.fromfile[n]:
            #                     coeff_l = np.zeros((n_em_cat*len(self.coeff_matrix[iname,:])))
            #                     for i_n_cat in range(n_em_cat):
            #                         coeff_l[i_n_cat:][::n_em_cat] = self.coeff_matrix[iname,:]

            #                     for l in range(self.nlag):
            #                         self.KG[l*self.nparams:(l+1)*self.nparams-n_bg_params] = np.multiply( self.KG[l*self.nparams:(l+1)*self.nparams-n_bg_params], coeff_l )

            #                     logging.info('Localized observation %i at station %s (nr. %i)'%(self.obs_ids[n],stationname,iname))

            #                     lfound = True

            #                     break

            #             if not lfound:
            #                 logging.info('Not localized observation %i as coefficient not found' %(self.obs_ids[n]))
            ###            if self.loc_L[n] > 0:
            ###                obslati, obsloni = self.find_coord_index(self.latitude[n],self.longitude[n],180,360)
            ###                for l in range(self.nlag):
            ###                    self.KG[l*self.nparams:(l+1)*self.nparams] = np.multiply(self.KG[l*self.nparams:(l+1)*self.nparams], self.loc_coeff[str(self.loc_L[n])][obslati,obsloni,:])
            ###                logging.debug('Localized observation %i with localization length %s' %(self.obs_ids[n], self.loc_L[n]))

            n_em_cat = 2
            if self.fromfile[n] in skip_stations:
                return  # Skip stations outside of the domain!

            coeff_l = np.zeros((n_em_cat * len(self.coeff_matrix[n, :])))
            for i_n_cat in range(n_em_cat):
                coeff_l[i_n_cat:][::n_em_cat] = self.coeff_matrix[n, :]

            for l in range(self.nlag):
                self.KG[l * self.nparams:(l + 1) * self.nparams -
                        n_bg_params] = np.multiply(
                            self.KG[l * self.nparams:(l + 1) * self.nparams -
                                    n_bg_params], coeff_l)

            logging.info('Localized observation %i at station %s (nr. %i)' %
                         (self.obs_ids[n], self.fromfile[n], n))

    def set_algorithm(self, algorithm='Serial'):
        """ determine which minimum least squares algorithm to use """

        if algorithm == 'Serial':
            self.algorithm = 'Serial'
        else:
            self.algorithm = 'Bulk'

        logging.info("Current minimum least squares algorithm is set to %s" %
                     self.algorithm)


################### End Class Optimizer ###################

if __name__ == "__main__":
    pass
