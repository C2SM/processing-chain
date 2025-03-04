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
# ct_statevector_tools.py

"""
.. module:: statevector
.. moduleauthor:: Wouter Peters 

Revision History:
File created on 28 Jul 2010.

The module statevector implements the data structure and methods needed to work with state vectors (a set of unknown parameters to be optimized by a DA system) of different lengths, types, and configurations. Two baseclasses together form a generic framework:
    * :class:`~da.baseclasses.statevector.StateVector`
    * :class:`~da.baseclasses.statevector.EnsembleMember`

As usual, specific implementations of StateVector objects are done through inheritance form these baseclasses. An example of designing 
your own baseclass StateVector we refer to :ref:`tut_chapter5`.

.. autoclass:: da.baseclasses.statevector.StateVector 

.. autoclass:: da.baseclasses.statevector.EnsembleMember 

"""

import os
import logging
import numpy as np
from scipy.linalg import cholesky
from datetime import timedelta
import datetime as dt
import da.tools.io4 as io
from multiprocessing import Pool
import xarray as xr
from sklearn.metrics.pairwise import haversine_distances

identifier = 'ICON Statevector '
version = '0.0'

################### Begin Class EnsembleMember ###################

class EnsembleMember(object):
    """ 
        An ensemble member object consists of:
           * a member number
           * parameter values
           * an observation object to hold sampled values for this member

        Ensemble members are initialized by passing only an ensemble member number, all data is added by methods 
        from the :class:`~da.baseclasses.statevector.StateVector`. Ensemble member objects have almost no functionality 
        except to write their data to file using method :meth:`~da.baseclasses.statevector.EnsembleMember.write_to_file`

        .. automethod:: da.baseclasses.statevector.EnsembleMember.__init__ 
        .. automethod:: da.baseclasses.statevector.EnsembleMember.write_to_file 
        .. automethod:: da.baseclasses.statevector.EnsembleMember.AddCustomFields 

    """

    def __init__(self, membernumber):
        """
           :param memberno: integer ensemble number
           :rtype: None

           An EnsembleMember object is initialized with only a number, and holds two attributes as containter for later
           data:
                * param_values, will hold the actual values of the parameters for this data
                * ModelSample, will hold an :class:`~da.baseclasses.obs.Observation` object and the model samples resulting from this members' data

        """
        self.membernumber = membernumber   # the member number
        self.param_values = None           # Parameter values of this member

################### End Class EnsembleMember ###################

################### Begin Class StateVector ###################


class StateVector(object):
    """ 
    The StateVector object first of all contains the data structure of a statevector, defined by 3 attributes that define the 
    dimensions of the problem in parameter space:
        * nlag
        * nparameters
        * nmembers

    The fourth important dimension `nobs` is not related to the StateVector directly but is initialized to 0, and later on 
    modified to be used in other parts of the pipeline:
        * nobs

    These values are set as soon as the :meth:`~da.baseclasses.statevector.StateVector.setup` is called from the :ref:`pipeline`. 
    Additionally, the value of attribute `isOptimized` is set to `False` indicating that the StateVector holds a-priori values 
    and has not been modified by the :ref:`optimizer`.

    StateVector objects can be filled with data in two ways
        1. By reading the data from file
        2. By creating the data through a set of method calls

    Option (1) is invoked using method :meth:`~da.baseclasses.statevector.StateVector.read_from_file`. 
    Option (2) consists of a call to method :meth:`~da.baseclasses.statevector.StateVector.make_new_ensemble`

    Once the StateVector object has been filled with data, it is used in the pipeline and a few more methods are
    invoked from there:
        * :meth:`~da.baseclasses.statevector.StateVector.propagate`, to advance the StateVector from t=t to t=t+1
        * :meth:`~da.baseclasses.statevector.StateVector.write_to_file`, to write the StateVector to a NetCDF file for later use

    The methods are described below:

    .. automethod:: da.baseclasses.statevector.StateVector.setup 
    .. automethod:: da.baseclasses.statevector.StateVector.read_from_file
    .. automethod:: da.baseclasses.statevector.StateVector.write_to_file
    .. automethod:: da.baseclasses.statevector.StateVector.make_new_ensemble
    .. automethod:: da.baseclasses.statevector.StateVector.propagate
    .. automethod:: da.baseclasses.statevector.StateVector.write_members_to_file

    Finally, the StateVector can be mapped to a gridded array, or to a vector of TransCom regions, using:

    .. automethod:: da.baseclasses.statevector.StateVector.grid2vector
    .. automethod:: da.baseclasses.statevector.StateVector.vector2grid
    .. automethod:: da.baseclasses.statevector.StateVector.vector2tc
    .. automethod:: da.baseclasses.statevector.StateVector.state2tc

    """

    def __init__(self):
        self.ID = identifier
        self.version = version

        # The following code allows the object to be initialized with a dacycle object already present. Otherwise, it can
        # be added at a later moment.

        logging.info('Statevector object initialized: %s' % self.ID)

    def setup(self, dacycle):
        """
        setup the object by specifying the dimensions. 
        There are two major requirements for each statvector that you want to build:
        
            (1) is that the statevector can map itself onto a regular grid
            (2) is that the statevector can map itself (mean+covariance) onto TransCom regions

        An example is given below.
        """

        self.nlag = int(dacycle['time.nlag'])
        self.nmembers = int(dacycle['da.optimizer.nmembers']) #number of ensemble members, e.g. 192 for the icon case
        self.nparams = int(dacycle.dasystem['nparameters']) #n_reg * n_tracers * n_categories + n_bg_params
        self.nobs = 0
        self.grid_fn = dacycle['icon_grid_path']
        
        self.obs_to_assimilate = ()  # empty containter to hold observations to assimilate later on

        # These list objects hold the data for each time step of lag in the system. Note that the ensembles for each time step consist 
        # of lists of EnsembleMember objects, we define member 0 as the mean of the distribution and n=1,...,nmembers as the spread.

        self.ensemble_members = list(range(self.nlag))

        for n in range(self.nlag):
            self.ensemble_members[n] = []

        #msteiner:
        self.isOptimized = False
        self.C = np.zeros((self.nparams,self.nparams))
        #---------



    def make_new_ensemble(self, lag, covariancematrix=None,n_bg_params=0):
        """ 
        :param lag: an integer indicating the time step in the lag order
        :param covariancematrix: a matrix to draw random values from
        :rtype: None
    
        Make a new ensemble, the attribute lag refers to the position in the state vector. 
        Note that lag=1 means an index of 0 in python, hence the notation lag-1 in the indexing below.
        The argument is thus referring to the lagged state vector as [1,2,3,4,5,..., nlag]

        The optional covariance object to be passed holds a matrix of dimensions [nparams, nparams] which is
        used to draw ensemblemembers from. If this argument is not passed it will ne substituted with an 
        identity matrix of the same dimensions.

        """    

        logging.info('msteiner: current lag: %i '%(lag))
        logging.info('msteiner: nlag; %i '%(self.nlag))
        categories = {max(lambdas)}
        if np.all(self.C==0.):
            logging.info('msteiner: performing cholesky decomposition')

            ds_grid = xr.open_dataset(self.grid_fn)
            grid_coords = np.stack([ds_grid['clat'].values, ds_grid['clon'].values], axis=-1)  # Radians
            distances = haversine_distances(grid_coords, grid_coords) * 6371.0
            logging.info('ekoene: computed distances matrix')
            covariancematrix = np.zeros((self.nparams,self.nparams), dtype=np.float32)
            
            {re.sub(fr'(?m)(?<={chr(10)})^', '            ', cfg.CTDAS_covariancematrix_definition.strip(), flags=re.MULTILINE)}

            self.C = np.linalg.cholesky(covariancematrix)
            del covariancematrix

        logging.info('Cholesky decomposition has finished')

        # Propagate mean values 
        newmean = np.ones(self.nparams, float) # standard value for a new time step is 1.0
        if lag == self.nlag - 1 and self.nlag >= 2:
            newmean += 2*self.ensemble_members[lag - 1][0].param_values
            newmean = newmean / 3.0


            #Propagate background mean state by 100%:
            if n_bg_params>0:
                newmean[self.nparams-n_bg_params:] = self.ensemble_members[lag - 1][0].param_values[self.nparams-n_bg_params:]


        ####### New forecast model for the mean: take 100% of the optimized value #######
        #newmean = np.ones(self.nparams, float) # standard value for a new time step is 1.0
        #if lag == self.nlag - 1 and self.nlag >= 2: #self.nlag >= 3:
        #    newmean -= 1.
        #    newmean += self.ensemble_members[lag - 1][0].param_values
        ####### --- #######

        #DEBUG newmean
        for cat in range(categories):
            logging.info('Category (%s) ' % str(cat + 1))
            logging.info('New mean (%s) ' % str(np.nanmean(newmean[cat:][::categories])))
        # Create the first ensemble member with a deviation of 0.0 and add to list
        newmember = EnsembleMember(0)
        newmember.param_values = newmean.flatten()  # no deviations
        self.ensemble_members[lag].append(newmember)

        # Create members 1:nmembers and add to ensemble_members list
        #np.random.normal(loc=1.0, scale=0.5, size=100)
        for member in range(1, self.nmembers):
            rands = np.random.randn(self.nparams)
            newmember = EnsembleMember(member)
            logging.info('pre-dot')
            # newmember.param_values = np.dot(self.C, rands) + newmean
            newmember.param_values = np.einsum("ij, j -> i", self.C, rands) + newmean
            logging.info('post-dot')
            self.ensemble_members[lag].append(newmember)
            logging.info('Created parameters for ensemble member %i'%(member))

        #DEBUG lambdas
        lambdas = np.array([])
        for member in range(0, self.nmembers):
            logging.info('Member shape (%s) ' % str(np.shape(self.ensemble_members[lag][member].param_values)))
            lambdas = np.append(lambdas, self.ensemble_members[lag][member].param_values)
        lambdas = np.reshape(lambdas, (self.nmembers, self.nparams))
        members_array = np.mean(lambdas, axis = 0)
        # logging.info('Member array shape (%s) ' % str(np.shape(members_array)))
        for cat in range(categories):
            logging.info('Category (%s) ' % str(cat + 1))
            logging.info('Lambda mean (%s) ' % str(np.nanmean(members_array[cat:][::categories])))

        #del C #msteiner: this line causes the "invalid pointer"-error at this point, otherwise it occurs after the code reached the end of this function

        logging.info('%d new ensemble members were added to the state vector # %d' % (self.nmembers, (lag + 1)))


    def propagate(self, dacycle, method='create_new_member', filename=None, date=None, initdir=None):
        """
        :rtype: None

        Propagate the parameter values in the StateVector to the next cycle. This means a shift by one cycle 
        step for all states that will
        be optimized once more, and the creation of a new ensemble for the time step that just 
        comes in for the first time (step=nlag). 
        In the future, this routine can incorporate a formal propagation of the statevector.

        """
        
        # Remove State Vector n=1 by simply "popping" it from the list and appending a new empty list at the front. This empty list will
        # hold the new ensemble for the new cycle 

        self.ensemble_members.pop(0)
        self.ensemble_members.append([])

        # And now create a new time step of mean + members for n=nlag
        if method == 'create_new_member':
            date = dacycle['time.start'] + timedelta(days=(self.nlag - 0.5) * int(dacycle['time.cycle']))
            cov = self.get_covariance(date, dacycle)
            self.make_new_ensemble(self.nlag - 1, cov,int(dacycle['statevector.bg_params']))

        elif method == 'read_new_member':
            if os.path.exists(filename):
                self.read_ensemble_member_from_file(filename, self.nlag-1, qual='opt', read_lag=0)
            else:
                self.read_ensemble_member_from_file(filename, self.nlag-1, date, initdir, qual='opt', read_lag=0)

        elif method == 'read_mean':
            date = dacycle['time.start'] + timedelta(days=(self.nlag - 0.5) * int(dacycle['time.cycle']))
            cov = self.get_covariance(date, dacycle)
            if os.path.exists(filename):
                meanstate = self.read_mean_from_file(filename, self.nlag-1, qual='opt')
            else:
                meanstate = self.read_mean_from_file(filename, self.nlag-1, date, initdir, qual='opt')
            self.make_new_ensemble(self.nlag - 1, cov, meanstate)

        logging.info('The state vector has been propagated by one cycle')


    def write_to_file(self, filename, qual):
        """
        :param filename: the full filename for the output NetCDF file
        :rtype: None

        Write the StateVector information to a NetCDF file for later use. 
        In principle the output file will have only one two datasets inside 
        called:
            * `meanstate`, dimensions [nlag, nparamaters]
            * `ensemblestate`, dimensions [nlag,nmembers, nparameters]

        This NetCDF information can be read back into a StateVector object using 
        :meth:`~da.baseclasses.statevector.StateVector.read_from_file`

        """
        #import da.tools.io4 as io
        #import da.tools.io as io

        if qual == 'prior':
            f = io.CT_CDF(filename, method='create')
            logging.debug('Creating new StateVector output file (%s)' % filename)
            #qual = 'prior'
        else:
            f = io.CT_CDF(filename, method='write')
            logging.debug('Opening existing StateVector output file (%s)' % filename)
            #qual = 'opt'

        dimparams = f.add_params_dim(self.nparams)
        dimmembers = f.add_members_dim(self.nmembers)
        dimlag = f.add_lag_dim(self.nlag, unlimited=True)

        for n in range(self.nlag):
            members = self.ensemble_members[n]
            mean_state = members[0].param_values

            savedict = f.standard_var(varname='meanstate_%s' % qual)
            savedict['dims'] = dimlag + dimparams 
            savedict['values'] = mean_state
            savedict['count'] = n
            savedict['comment'] = 'this represents the mean of the ensemble'
            f.add_data(savedict)

            members = self.ensemble_members[n]
            devs = np.asarray([m.param_values.flatten() for m in members])
            data = devs - np.asarray(mean_state)

            savedict = f.standard_var(varname='ensemblestate_%s' % qual)
            savedict['dims'] = dimlag + dimmembers + dimparams 
            savedict['values'] = data
            savedict['count'] = n
            savedict['comment'] = 'this represents deviations from the mean of the ensemble'
            f.add_data(savedict)
        f.close()

        logging.info('Successfully wrote the State Vector to file (%s) ' % filename)



    def interpolate_mean_ensemble(self, initdir, date, qual='opt', readensemble=True):
        # deduce window length of source run:
        all_dates = os.listdir(initdir)
        for i, dstr in enumerate(all_dates):
            all_dates[i] = dt.datetime.strptime(dstr,'%Y%m%d')
        del i, dstr
        all_dates = sorted(all_dates)
        ddays = (all_dates[1]-all_dates[0]).days
        del all_dates

        # find dates in source directory just before and after target date
        found_datemin, found_datemax = False, False
        for d in range(ddays):
            datei = date - dt.timedelta(days=d)
            if not found_datemin and os.path.exists(os.path.join(initdir, datei.strftime('%Y%m%d'), 'savestate_%s.nc'%datei.strftime('%Y%m%d'))):
                datemin = datei
                found_datemin = True

            datei = date + dt.timedelta(days=d)
            if not found_datemax and os.path.exists(os.path.join(initdir, datei.strftime('%Y%m%d'), 'savestate_%s.nc'%datei.strftime('%Y%m%d'))):
                datemax = datei
                found_datemax = True

            if found_datemin and found_datemax:
                print('Found datemin = %s and datemax = %s' %(datemin.strftime('%Y%m%d'), datemax.strftime('%Y%m%d')))
                break
        del d
        logging.debug('Ensemble for %s will be interpolated from %s and %s' %(date.strftime('%Y-%m-%d'), datemin.strftime('%Y-%m-%d'),datemax.strftime('%Y-%m-%d')))

        # Read ensemble from both files
        filename1 = os.path.join(initdir, datemin.strftime('%Y%m%d'), 'savestate_%s.nc'%datemin.strftime('%Y%m%d'))
        f = io.ct_read(filename1, 'read')
        meanstate1  = f.get_variable('statevectormean_' + qual)     # [nlag x nparameters]
        if readensemble:
            ensmembers1 = f.get_variable('statevectorensemble_' + qual) # [nlag x nmembers x nparameters]
        f.close()

        filename2 = os.path.join(initdir, datemax.strftime('%Y%m%d'), 'savestate_%s.nc'%datemax.strftime('%Y%m%d'))
        f = io.ct_read(filename2, 'read')
        meanstate2  = f.get_variable('statevectormean_' + qual)     # [nlag x nparameters]
        if readensemble:
            ensmembers2 = f.get_variable('statevectorensemble_' + qual) # [nlag x nmembers x nparameters]
        f.close()

        # interpolate mean and ensemble between datemin and datemax
        meanstate  = ((datemax-date).days/ddays)*meanstate1 + ((date-datemin).days/ddays)*meanstate2
        if readensemble:
            ensmembers = ((datemax-date).days/ddays)*ensmembers1 + ((date-datemin).days/ddays)*ensmembers2
            return meanstate, ensmembers

        else:
            return meanstate



    def read_mean_from_file(self, filename, lag, date=None, initdir=None, qual='opt'):
        if date is None:
            f = io.ct_read(filename, 'read')
            meanstate  = f.get_variable('statevectormean_' + qual)     # [nlag x nparameters]
            f.close
        else:
            meanstate = self.interpolate_mean_ensemble(initdir, date, qual, readensemble=False)

        logging.info('Successfully read the mean state vector from file (%s) ' %filename)

        return meanstate[lag,:]



    def read_ensemble_member_from_file(self, filename, lag, date=None, initdir=None, qual='opt', read_lag=0):

        # if date is None we can directly read mean and ensemble members. Else we will need to read 2 ensembles and interpolate
        if date is None:
            f = io.ct_read(filename, 'read')
            meanstate  = f.get_variable('statevectormean_' + qual)     # [nlag x nparameters]
            ensmembers = f.get_variable('statevectorensemble_' + qual) # [nlag x nmembers x nparameters]
            f.close()

        else:
            meanstate, ensmembers = self.interpolate_mean_ensemble(initdir, date, qual, readensemble=True)

        # add to statevector
        if not self.ensemble_members[lag] == []:
            self.ensemble_members[lag] = []
            logging.warning('Existing ensemble for lag=%d was removed to make place for newly read data' % (n + 1))

        for m in range(self.nmembers):
            newmember = EnsembleMember(m)
            newmember.param_values = ensmembers[read_lag, m, :].flatten() + meanstate[read_lag,:]  # add the mean to the deviations to hold the full parameter values
            self.ensemble_members[lag].append(newmember)

        logging.info('Successfully read the State Vector for lag %s from file (%s) ' % (lag,filename))




    def read_from_file(self, filename, qual='opt'):
        """ 
        :param filename: the full filename for the input NetCDF file
        :param qual: a string indicating whether to read the 'prior' or 'opt'(imized) StateVector from file
        :rtype: None

        Read the StateVector information from a NetCDF file and put in a StateVector object
        In principle the input file will have only one four datasets inside 
        called:
            * `meanstate_prior`, dimensions [nlag, nparamaters]
            * `ensemblestate_prior`, dimensions [nlag,nmembers, nparameters]
            * `meanstate_opt`, dimensions [nlag, nparamaters]
            * `ensemblestate_opt`, dimensions [nlag,nmembers, nparameters]

        This NetCDF information can be written to file using 
        :meth:`~da.baseclasses.statevector.StateVector.write_to_file`

        """

        #import da.tools.io as io
        f = io.ct_read(filename, 'read')
        meanstate = f.get_variable('statevectormean_' + qual)
        ensmembers = f.get_variable('statevectorensemble_' + qual)
        f.close()

        for n in range(self.nlag):
            if not self.ensemble_members[n] == []:
                self.ensemble_members[n] = []
                logging.warning('Existing ensemble for lag=%d was removed to make place for newly read data' % (n + 1))

            for m in range(self.nmembers):
                newmember = EnsembleMember(m)
                newmember.param_values = ensmembers[n, m, :].flatten() + meanstate[n]  # add the mean to the deviations to hold the full parameter values
                self.ensemble_members[n].append(newmember)

        logging.info('Successfully read the State Vector from file (%s) ' % filename)

    def write_members_to_file(self, lag, outdir, endswith='.nc', obsoperator=None):
        """
           :param: lag: Which lag step of the filter to write, must lie in range [1,...,nlag]
           :param: outdir: Directory where to write files
           :param: endswith: Optional label to add to the filename, default is simply .nc
           :rtype: None

           Write ensemble member information to a NetCDF file for later use. The standard output filename is 
           *parameters.DDD.nc* where *DDD* is the number of the ensemble member. Standard output file location 
           is the `dir.input` of the dacycle object. In principle the output file will have only two datasets inside 
           called `parametervalues` which is of dimensions `nparameters` and `parametermap` which is of dimensions (180,360). 
           This dataset can be read and used by a :class:`~da.baseclasses.observationoperator.ObservationOperator` object. 

           .. note:: if more, or other information is needed to complete the sampling of the ObservationOperator you
                     can simply inherit from the StateVector baseclass and overwrite this write_members_to_file function.

        """

        # These import statements caused a crash in netCDF4 on MacOSX. No problems on Jet though. Solution was
        # to do the import already at the start of the module, not just in this method.
           
        #import da.tools.io as io
        #import da.tools.io4 as io

        members = self.ensemble_members[lag]

        for mem in members:
            filename = os.path.join(outdir, 'parameters.%03d%s' % (mem.membernumber, endswith))
            ncf = io.CT_CDF(filename, method='create')
            dimparams = ncf.add_params_dim(self.nparams)
            dimgrid = ncf.add_latlon_dim()

            data = mem.param_values

            savedict = io.std_savedict.copy()
            savedict['name'] = "parametervalues"
            savedict['long_name'] = "parameter_values_for_member_%d" % mem.membernumber
            savedict['units'] = "unitless"
            savedict['dims'] = dimparams 
            savedict['values'] = data
            savedict['comment'] = 'These are parameter values to use for member %d' % mem.membernumber
            ncf.add_data(savedict)

            griddata = self.vector2grid(vectordata=data)

            savedict = io.std_savedict.copy()
            savedict['name'] = "parametermap"
            savedict['long_name'] = "parametermap_for_member_%d" % mem.membernumber
            savedict['units'] = "unitless"
            savedict['dims'] = dimgrid 
            savedict['values'] = griddata.tolist()
            savedict['comment'] = 'These are gridded parameter values to use for member %d' % mem.membernumber
            ncf.add_data(savedict)

            ncf.close()

            logging.debug('Successfully wrote data from ensemble member %d to file (%s) ' % (mem.membernumber, filename))


    def get_covariance(self, date, cycleparams):
        pass
    
################### End Class StateVector ###################

if __name__ == "__main__":
    pass

