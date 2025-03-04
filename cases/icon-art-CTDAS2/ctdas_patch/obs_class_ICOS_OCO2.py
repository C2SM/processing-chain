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
# obs.py
"""
.. module:: obs
.. moduleauthor:: Wouter Peters 

Revision History:
File created on 28 Jul 2010.

.. autoclass:: da.baseclasses.obs.Observations 
   :members: setup, Validate, add_observations, add_simulations, add_model_data_mismatch, write_sample_coords  

.. autoclass:: da.baseclasses.obs.ObservationList 
   :members: __init__

"""

import logging, sys, os
from numpy import array, ndarray
import datetime as dt
from datetime import timedelta
import numpy as np
from netCDF4 import Dataset
import xarray as xr
from multiprocessing import Pool
import datetime

sys.path.append(os.getcwd())
sys.path.append('../../')

print("Python Version:", sys.version)
print("Python Executable Path:", sys.executable)

from unidecode import unidecode

identifier = 'Observations baseclass'
version = '0.0'

from da.observations.obs_baseclass import Observations
import da.tools.io4 as io
import da.tools.rc as rc

################### Begin Class Observations ###################


class ICOSObservations(object):
    """ 
    The baseclass Observations is a generic object that provides a number of methods required for any type of observations used in 
    a data assimilation system. These methods are called from the CarbonTracker pipeline. 

    .. note:: Most of the actual functionality will need to be provided through a derived Observations class with the methods 
              below overwritten. Writing your own derived class for Observations is one of the first tasks you'll likely 
              perform when extending or modifying the CarbonTracker Data Assimilation Shell.

    Upon initialization of the class, an object is created that holds no actual data, but has a placeholder attribute `self.Data` 
    which is an empty list of type :class:`~da.baseclasses.obs.ObservationList`. An ObservationList object is created when the 
    method :meth:`~da.baseclasses.obs.Observations.add_observations` is invoked in the pipeline. 

    From the list of observations, a file is written  by method 
    :meth:`~da.baseclasses.obs.Observations.write_sample_info`
    with the sample info needed by the 
    :class:`~da.baseclasses.observationoperator.ObservationOperator` object. The values returned after sampling 
    are finally added by :meth:`~da.baseclasses.obs.Observations.add_simulations`

    """

    def __init__(self):
        """
        create an object with an identifier, version, and an empty ObservationList
        """
        self.ID = identifier
        self.version = version
        self.datalist = []  # initialize with an empty list of obs

        # The following code allows the object to be initialized with a dacycle object already present. Otherwise, it can
        # be added at a later moment.

        logging.info('Observations object initialized: %s' % self.ID)

    def getlength(self):
        return len(self.datalist)

    def setup(self, dacycle):
        """ Perform all steps needed to start working with observational data, this can include moving data, concatenating files,
            selecting datasets, etc.
        """

        self.startdate = dacycle['time.sample.start']
        self.enddate = dacycle['time.sample.end']
        op_dir = dacycle.dasystem['obs.input.dir']
        #self.obs_fname = dacycle.dasystem['obs.input.fname']
        self.n_bg_params = int(dacycle['statevector.bg_params'])
        self.tracer = str(dacycle['statevector.tracer'])
        if not os.path.exists(op_dir):
            msg = 'Could not find  the required ObsPack distribution (%s) ' % op_dir
            logging.error(msg)
            raise IOError(msg)
        else:
            self.obspack_dir = op_dir

        self.datalist = []

    def get_samples_type(self):
        return 'insitu'

    def add_observations(self):
        """ 
        Add actual observation data to the Observations object. This is in a form of an 
        :class:`~da.baseclasses.obs.ObservationList` that is contained in self.Data. The 
        list has as only requirement that it can return the observed+simulated values 
        through the method :meth:`~da.baseclasses.obs.ObservationList.getvalues`

        """

        # Step 1: Read list of available site files in package
        ######################################################

        mdm_dict = {{}}

        mountain_stations = [
            'Jungfraujoch_5', 'Monte Cimone_8', 'Puy de Dome_10',
            'Pic du Midi_28', 'Zugspitze_3', 'Hohenpeissenberg_50',
            'Hohenpeissenberg_93', 'Hohenpeissenberg_131', 'Schauinsland_12',
            'Plateau Rosa_10'
        ]
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
            'Ispra_40',
            'Ispra_60',
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

        os.environ["HDF5_USE_FILE_LOCKING"] = "FALSE"

        ##########################################################################################################
        # THE FOLLOWING COMMENTED BLOCK IS FOR READING-IN REAL DATA
        ##########################################################################################################
        c_offset = 2  # ppm

        mdm_dictionary = {
            {
                "Beromunster_212": 6.2383423 + c_offset,
                "Bilsdale_248": 3.8534036 + c_offset,
                "Biscarrosse_47": 3.5997221 + c_offset,
                "Cabauw_207": 6.6093283 + c_offset,
                "Carnsore Point_14": 2.1894007 + c_offset,
                "Ersa_40": 2.3997285 + c_offset,
                "Gartow_341": 4.570544 + c_offset,
                "Heidelberg_30": 8.660628 + c_offset,
                "Hohenpeissenberg_131": 4.0553513 + c_offset,
                "Hyltemossa_150": 3.485432 + c_offset,
                "Ispra_100": 9.612817 + c_offset,
                "Jungfraujoch_5": 1.0802848 + c_offset,
                "Karlsruhe_200": 8.05013 + c_offset,
                "Kresin u Pacova_250": 3.829324 + c_offset,
                "La Muela_80": 3.2093291 + c_offset,
                "Laegern-Hochwacht_32": 9.556924 + c_offset,
                "Lindenberg_98": 5.4387555 + c_offset,
                "Lutjewad_60": 5.651525 + c_offset,
                "Monte Cimone_8": 1.7325112 + c_offset,
                "Observatoire de Haute Provence_100": 4.146905 + c_offset,
                "Observatoire perenne de l'environnement_120":
                6.8854113 + c_offset,
                "Pic du Midi_28": 1.2196398 + c_offset,
                "Plateau Rosa_10": 1.3211231 + c_offset,
                "Puy de Dome_10": 3.4529948 + c_offset,
                "Ridge Hill_90": 5.0861707 + c_offset,
                "Saclay_100": 6.8669567 + c_offset,
                "Schauinsland_12": 3.7896755 + c_offset,
                "Tacolneston_185": 4.6675706 + c_offset,
                "Torfhaus_147": 4.622525 + c_offset,
                "Trainou_180": 5.821612 + c_offset,
                "Weybourne_10": 4.4674397 + c_offset,
                "Zugspitze_3": 1.6796716 + c_offset
            }
        }  # Based on the simulated standard deviation of the signal (without background) over a full year.

        u_id = 1
        for ncfile in os.listdir(self.obspack_dir):
            if not ncfile.endswith('.nc'): continue

            logging.info('Found file ', )
            infile = os.path.join(self.obspack_dir, ncfile)
            print("infile = ", infile)

            f = xr.open_dataset(infile)

            logging.info('Looking into the file %s...' % (infile))

            sites_names = np.array(
                [unidecode(x) for x in f.Stations_names.values])

            mountain_hours = ['0' + str(x) for x in np.arange(0, 7)]
            rest_hours = [str(x) for x in np.arange(12, 17)]

            st_ind = np.arange(len(sites_names))

            #def caculate_interval_mean_cnc(dates, conc, hr):

            for x in st_ind:

                station_name = sites_names[x]
                if station_name in skip_stations:
                    continue  # Skip stations outside of the domain!

                cnc = f.Concentration[x].values

                dates = f.Dates[x].values
                flag = 1
                mdm_value = mdm_dictionary[
                    station_name]  # ERIK: CHANGED FROM constant 10
                height = f.Stations_masl[x].values
                lon = f.Lon[x].values
                lat = f.Lat[x].values
                species = self.tracer
                strategy = 1

                # ERIK: What is the influence of using UTC here? 00 UTC is already 1 o clock in the Netherlands? BUT, simulation is not w.r.t. UTC (or is it?).
                if station_name in mountain_stations:
                    ind_dt = np.asarray([
                        str(x)[11:13] in mountain_hours
                        for x in f.Dates.values[x]
                    ])  #mask of hours taken for mountain sites
                    hr = 0
                else:
                    ind_dt = np.asarray([
                        str(x)[11:13] in rest_hours for x in f.Dates.values[x]
                    ])  #mask of hours taken for the rest of the sites
                    hr = 12
                data = cnc[ind_dt]
                times = np.asarray([
                    datetime.datetime.strptime(str(x)[:-13], "%Y-%m-%dT%H:%M")
                    for x in dates[ind_dt]
                ])
                logging.info('Check dates: %s %s' %
                             (self.enddate + timedelta(days=1),
                              self.startdate + timedelta(days=1)))
                mask_da_interval = np.logical_and(
                    times <= (self.enddate + timedelta(days=1)),
                    (self.startdate + timedelta(days=1)) <= times)
                times = times[mask_da_interval]
                data = data[mask_da_interval]
                if len(times) > 0:
                    for iday in set([ii.day for ii in times]):
                        ids = [
                            iii for iii, dd in enumerate(times)
                            if dd.day == iday
                        ]
                        value = np.nanmean(
                            np.array([
                                c for i, c in enumerate(data)
                                if times[i].day == iday
                            ]))
                        dict_date = times[ids[0]].replace(hour=hr)
                        if not np.isfinite(value): continue

                        self.datalist.append(
                            MoleFractionSample(u_id, dict_date, station_name,
                                               value, 0.0, 0.0, 0.0, mdm_value,
                                               flag, height, lat, lon,
                                               station_name, species, strategy,
                                               0.0, station_name))

                        logging.info(
                            'For itime([day]T[hour]) (%iT%i) adding synthetic obs %i at station %s: %5.2e'
                            % (dict_date.day, dict_date.hour, u_id,
                               station_name, value))
                        u_id += 1

            # add_station_data_to_sample(x)

        logging.info("Observations list now holds %d values" %
                     len(self.datalist))
##########################################################################################################

    def add_simulations(self, filename, silent=False):
        """ Add the simulation data to the Observations object. 
        """

        if not os.path.exists(filename):
            msg = "Sample output filename for observations could not be found : %s" % filename
            logging.error(msg)
            logging.error("Did the sampling step succeed?")
            logging.error("...exiting")
            raise IOError(msg)

        ncf = io.ct_read(filename, method='read')
        ids = ncf.get_variable('obs_num')
        simulated = ncf.get_variable('flask')
        ncf.close()
        logging.info("Successfully read data from model sample file (%s)" %
                     filename)

        obs_ids = self.getvalues('id').tolist()
        ids = list(map(int, ids))

        missing_samples = []

        for idx, val in zip(ids, simulated):
            if idx in obs_ids:
                index = obs_ids.index(idx)
                self.datalist[index].simulated = val  # in mol/mol
            else:
                missing_samples.append(idx)

        if not silent and missing_samples != []:
            logging.warning(
                'Model samples were found that did not match any ID in the observation list. Skipping them...'
            )
            msg = '%s' % missing_samples
            logging.warning(msg)

        logging.debug("Added %d simulated values to the Data list" %
                      (len(ids) - len(missing_samples)))
        logging.info("Added %d simulated values to the Data list" %
                     (len(ids) - len(missing_samples)))

    def add_model_data_mismatch(self, filename):
        """ 
            Get the model-data mismatch values for this cycle.
        """
        self.rejection_threshold = 10.0  # 3-sigma cut-off
        self.global_R_scaling = 1.0  # no scaling applied

        for obs in self.datalist:  # first loop over all available data points to set flags correctly

            obs.may_localize = True  #False
            obs.may_reject = False
            obs.flag = 0

        logging.debug("Added Model Data Mismatch to all samples ")

    def write_sample_coords(self, obsinputfile):
        """ 
            Write the information needed by the observation operator to a file. Return the filename that was written for later use
        """

        if len(self.datalist) == 0:
            logging.debug(
                "No observations found for this time period, nothing written to obs file"
            )
        else:
            f = io.CT_CDF(obsinputfile, method='create')
            logging.debug(
                'Creating new observations file for ObservationOperator (%s)' %
                obsinputfile)

            dimid = f.add_dim('obs', len(self.datalist))
            #            dim200char = f.add_dim('string_of200chars', 200)
            dim50char = f.add_dim('string_of50chars', 50)
            dim3char = f.add_dim('string_of3chars', 3)
            dimcalcomp = f.add_dim('calendar_components', 6)

            data = self.getvalues('id')

            savedict = io.std_savedict.copy()
            savedict['name'] = "obs_num"
            savedict['dtype'] = "int"
            savedict['long_name'] = "Unique_Dataset_observation_index_number"
            savedict['units'] = ""
            savedict['dims'] = dimid
            savedict['values'] = data.tolist()
            savedict[
                'comment'] = "Unique index number within this dataset ranging from 0 to UNLIMITED."
            f.add_data(savedict)

            data = self.getvalues('evn')
            # print("data==", data)

            savedict = io.std_savedict.copy()
            savedict['name'] = "evn"
            savedict['dtype'] = "char"
            savedict['long_name'] = "Site_name_abbreviation"
            savedict['units'] = ""
            savedict['dims'] = dimid + dim50char
            savedict['values'] = data.tolist()
            savedict['comment'] = "Site name abbreviation as in the data file."
            f.add_data(savedict)

            data = self.getvalues('fromfile')

            savedict = io.std_savedict.copy()
            savedict['name'] = "fromfile"
            savedict['dtype'] = "char"
            savedict['long_name'] = "data_file_name"
            savedict['units'] = ""
            savedict['dims'] = dimid + dim50char
            savedict['values'] = data.tolist()
            savedict['comment'] = "File name of data file."
            f.add_data(savedict)

            data = [[d.year, d.month, d.day, d.hour, d.minute, d.second]
                    for d in self.getvalues('xdate')]

            savedict = io.std_savedict.copy()
            savedict['dtype'] = "int"
            savedict['name'] = "date_components"
            savedict['units'] = "integer components of UTC date/time"
            savedict['dims'] = dimid + dimcalcomp
            savedict['values'] = data
            savedict['missing_value'] = -999
            savedict[
                'comment'] = "Calendar date components as integers. Times and dates are UTC."
            savedict['order'] = "year, month, day, hour, minute, second"
            f.add_data(savedict)

            data = self.getvalues('lat')

            savedict = io.std_savedict.copy()
            savedict['dtype'] = "float"
            savedict['name'] = "latitude"
            savedict['units'] = "degrees_north"
            savedict['dims'] = dimid
            savedict['values'] = data.tolist()
            savedict['missing_value'] = -999.9
            f.add_data(savedict)

            data = self.getvalues('lon')

            savedict = io.std_savedict.copy()
            savedict['dtype'] = "float"
            savedict['name'] = "longitude"
            savedict['units'] = "degrees_east"
            savedict['dims'] = dimid
            savedict['values'] = data.tolist()
            savedict['missing_value'] = -999.9
            f.add_data(savedict)

            data = self.getvalues('height')

            savedict = io.std_savedict.copy()
            savedict['dtype'] = "float"
            savedict['name'] = "altitude"
            savedict['units'] = "meters_above_sea_level"
            savedict['dims'] = dimid
            savedict['values'] = data.tolist()
            savedict['missing_value'] = -999.9
            f.add_data(savedict)

            data = self.getvalues('samplingstrategy')

            savedict = io.std_savedict.copy()
            savedict['dtype'] = "int"
            savedict['name'] = "sampling_strategy"
            savedict['units'] = "NA"
            savedict['dims'] = dimid
            savedict['values'] = data.tolist()
            savedict['missing_value'] = -999
            f.add_data(savedict)

            data = self.getvalues('obs')

            savedict = io.std_savedict.copy()
            savedict['dtype'] = "float"
            savedict['name'] = "observed"
            savedict['long_name'] = "observedvalues"
            savedict['units'] = "mol mol-1"
            savedict['dims'] = dimid
            savedict['values'] = data.tolist()
            savedict['comment'] = 'Observations used in optimization'
            f.add_data(savedict)

            data = self.getvalues('mdm')

            savedict = io.std_savedict.copy()
            savedict['dtype'] = "float"
            savedict['name'] = "modeldatamismatch"
            savedict['long_name'] = "modeldatamismatch"
            savedict['units'] = "[mol mol-1]"
            savedict['dims'] = dimid
            savedict['values'] = data.tolist()
            savedict[
                'comment'] = 'Standard deviation of mole fractions resulting from model-data mismatch'
            f.add_data(savedict)
            f.close()

            logging.debug("Successfully wrote data to obs file")
            logging.info(
                "Sample input file for obs operator now in place [%s]" %
                obsinputfile)

    def write_sample_auxiliary(self, auxoutputfile):
        """ 
            Write selected additional information contained in the Observations object to a file for later processing. 

        """

    def getvalues(self, name, constructor=array):

        result = constructor([getattr(o, name) for o in self.datalist])
        if isinstance(result, ndarray):
            return result.squeeze()
        else:
            return result


################### End Class Observations ###################

################### Begin Class MoleFractionSample ###################


class MoleFractionSample(object):
    """ 
        Holds the data that defines a mole fraction Sample in the data assimilation framework. Sor far, this includes all
        attributes listed below in the __init__ method. One can additionally make more types of data, or make new
        objects for specific projects.

    """

    def __init__(self,
                 idx,
                 xdate,
                 code='XXX',
                 obs=0.0,
                 simulated=0.0,
                 resid=0.0,
                 hphr=0.0,
                 mdm=0.0,
                 flag=0,
                 height=0.0,
                 lat=-999.,
                 lon=-999.,
                 evn='0000',
                 species='co2',
                 samplingstrategy=1,
                 sdev=0.0,
                 fromfile='none.nc'):
        self.code = code.strip(
        )  # dataset identifier, i.e., co2_lef_tower_insitu_1_99
        self.xdate = xdate  # Date of obs
        self.obs = obs  # Value observed
        self.simulated = simulated  # Value simulated by model
        self.resid = resid  # Mole fraction residuals
        self.hphr = hphr  # Mole fraction prior uncertainty from fluxes and (HPH) and model data mismatch (R)
        self.mdm = mdm  # Model data mismatch
        self.may_localize = True  # Whether sample may be localized in optimizer
        self.may_reject = True  # Whether sample may be rejected if outside threshold
        self.flag = flag  # Flag
        self.height = height  # Sample height in masl
        self.lat = lat  # Sample lat
        self.lon = lon  # Sample lon
        self.id = idx  # Obspack ID within distrution (integer), e.g., 82536
        self.evn = evn  # Obspack Number within distrution (string), e.g., obspack_co2_1_PROTOTYPE_v0.9.2_2012-07-26_99_82536
        self.sdev = sdev  # standard deviation of ensemble
        self.masl = True  # Sample is in Meters Above Sea Level
        self.mag = not self.masl  # Sample is in Meters Above Ground
        self.species = species.strip()
        self.samplingstrategy = samplingstrategy
        self.fromfile = fromfile  # netcdf filename inside ObsPack distribution, to write back later


################### End Class MoleFractionSample ###################


################### Begin Class TotalColumnSample ###################
class TotalColumnSample(object):
    """
        Holds the data that defines a total column sample in the data assimilation framework. Sor far, this includes all
        attributes listed below in the __init__ method. One can additionally make more types of data, or make new
        objects for specific projects.
        This file may contain OCO-2 specific parts...
    """

    def __init__(self, idx, codex, xdate, obs=0.0, simulated=0.0, lat=-999., lon=-999., mdm=None, prior=0.0, prior_profile=0.0, av_kernel=0.0, pressure=0.0, \
                ##### freum vvvv

                pressure_weighting_function=None,
                ##### freum ^^^^
                level_def = "pressure_boundary", psurf = float('nan'), resid=0.0, hphr=0.0, flag=0, species='co2', sdev=0.0, \
                ##### freum vvvv

                latc_0=None, latc_1=None, latc_2=None, latc_3=None, lonc_0=None, lonc_1=None, lonc_2=None, lonc_3=None \
                ##### freum ^^^^

                ):
        self.id = idx  # Sounding ID
        self.code = codex  # Retrieval ID
        self.xdate = xdate  # Date of obs
        self.obs = obs  # Value observed
        self.simulated = simulated  # Value simulated by model, fillvalue = -9999
        self.lat = lat  # Sample lat
        self.lon = lon  # Sample lon
        ##### freum vvvv
        self.latc_0 = latc_0  # Sample latitude corner
        self.latc_1 = latc_1  # Sample latitude corner
        self.latc_2 = latc_2  # Sample latitude corner
        self.latc_3 = latc_3  # Sample latitude corner
        self.lonc_0 = lonc_0  # Sample longitude corner
        self.lonc_1 = lonc_1  # Sample longitude corner
        self.lonc_2 = lonc_2  # Sample longitude corner
        self.lonc_3 = lonc_3  # Sample longitude corner
        ##### freum ^^^^
        self.mdm = mdm  # Model data mismatch
        self.prior = prior  # A priori column value used in retrieval
        self.prior_profile = prior_profile  # A priori profile used in retrieval
        self.av_kernel = av_kernel  # Averaging kernel
        self.pressure = pressure  # Pressure levels of retrieval
        # freum vvvv
        self.pressure_weighting_function = pressure_weighting_function  # Pressure weighting function
        # freum ^^^^
        self.level_def = level_def  # Are prior and averaging kernel defined as layer averages?
        self.psurf = psurf  # Surface pressure (only needed if level_def is "layer_average")
        self.loc_L = int(
            600
        )  #int(0)            # freum 2021-07-13: insert this dummy value so the code runs with the current version of CTDAS. *Should* not affect results if localizetype == "CT2007" as in all my runs. However, replace this file with the standard observation file, obs_column_xco2.py

        self.resid = resid  # Mole fraction residuals
        self.hphr = hphr  # Mole fraction prior uncertainty from fluxes and (HPH) and model data mismatch (R)
        self.may_localize = True  # Whether sample may be localized in optimizer
        self.may_reject = True  # Whether sample may be rejected if outside threshold
        self.flag = flag  # Flag
        self.sdev = sdev  # standard deviation of ensemble
        self.species = species.strip()


################### End Class TotalColumnSample ###################

################### Begin Class TotalColumnObservations ###################


class TotalColumnObservations(Observations):
    """ An object that holds data + methods and attributes needed to manipulate column samples
    """

    def setup(self, dacycle):

        self.startdate = dacycle['time.sample.start'] + timedelta(days=1)
        self.enddate = dacycle['time.sample.end']

        # Path to the input data (daily files)
        sat_files = dacycle.dasystem['obs.column.ncfile'].split(',')
        sat_dirs = dacycle.dasystem['obs.column.input.dir'].split(',')

        self.sat_dirs = []
        self.sat_files = []
        for i in range(len(sat_dirs)):
            if not os.path.exists(sat_dirs[i].strip()):
                msg = 'Could not find the required satellite input directory (%s) ' % sat_dirs[
                    i]
                logging.error(msg)
                raise IOError(msg)
            else:
                self.sat_dirs.append(sat_dirs[i].strip())
                self.sat_files.append(sat_files[i].strip())
        del i

        # Get observation selection criteria (if present):
        if 'obs.column.selection.variables' in dacycle.dasystem.keys(
        ) and 'obs.column.selection.criteria' in dacycle.dasystem.keys():
            self.selection_vars = dacycle.dasystem[
                'obs.column.selection.variables'].split(',')
            self.selection_criteria = dacycle.dasystem[
                'obs.column.selection.criteria'].split(',')
            logging.debug('Data selection criteria found: %s, %s' %
                          (self.selection_vars, self.selection_criteria))
        else:
            self.selection_vars = []
            self.selection_criteria = []
            logging.info(
                'No data observation selection criteria found, using all observations in file.'
            )

        # Model data mismatch approach
        # self.mdm_calculation = dacycle.dasystem.get('mdm.calculation')
        # logging.debug('mdm.calculation = %s' %self.mdm_calculation)
        # if not self.mdm_calculation in ['parametrization','empirical','no_transport_error']:
        #     logging.warning('No valid model data mismatch method found. Valid options are \'parametrization\' and \'empirical\'. ' + \
        #                     'Using a constant estimate for the model uncertainty of 1ppm everywhere.')
        # else:
        #     logging.info('Model data mismatch approach = %s' %self.mdm_calculation)

        # Path to file with observation error settings for column observations
        # Currently the same settings for all assimilated retrieval products: should this be one file per product?
        logging.debug('Skipping obs.column.rc check!')
        # if not os.path.exists(dacycle.dasystem['obs.column.rc']):
        #     msg = 'Could not find the required column observation .rc input file (%s) ' % dacycle.dasystem['obs.column.rc']
        #     logging.debug(msg)
        #     logging.debug('...but continuing!')
        #     # logging.error(msg)
        #     # raise IOError(msg)
        # else:
        #     self.obs_file = (dacycle.dasystem['obs.column.rc'])

        self.datalist = []

        # Switch to indicate whether simulated column samples are read from obsOperator output,
        # or whether the sampling is done within CTDAS (in obsOperator class)
        self.sample_in_ctdas = dacycle.dasystem[
            'sample.in.ctdas'] if 'sample.in.ctdas' in dacycle.dasystem.keys(
            ) else False
        logging.debug('sample.in.ctdas = %s' % self.sample_in_ctdas)

    def get_samples_type(self):
        return 'column'

    def add_observations(self):
        """ Reading of total column observations, and selection of observations that will be sampled and assimilated.

        """

        # Read observations from daily input files
        for i in range(len(self.sat_dirs)):

            logging.info('Reading observations from %s' %
                         os.path.join(self.sat_dirs[i], self.sat_files[i]))

            infile0 = os.path.join(self.sat_dirs[i], self.sat_files[i])
            ndays = 0

            while self.startdate + dt.timedelta(days=ndays) <= self.enddate:

                infile = infile0.replace(
                    "<YYYYMMDD>",
                    (self.startdate +
                     dt.timedelta(days=ndays)).strftime("%Y%m%d"))
                logging.info('To be precise, reading observations from %s' %
                             infile)

                if os.path.exists(infile):

                    logging.info("Reading observations for %s" %
                                 (self.startdate +
                                  dt.timedelta(days=ndays)).strftime("%Y%m%d"))
                    len_init = len(self.datalist)

                    # get index of observations that satisfy selection criteria (based on variable names and values in system rc file, if present)
                    ncf = io.ct_read(infile, 'read')
                    if self.selection_vars:
                        selvars = []
                        for j in self.selection_vars:
                            selvars.append(ncf.get_variable(j.strip()))
                        del j
                        criteria = []
                        for j in range(len(self.selection_vars)):
                            criteria.append(
                                eval('selvars[j]' +
                                     self.selection_criteria[j]))
                        del j
                        #criteria  = [eval('selvars[i]'+self.selection_criteria[i]) for i in range(len(self.selection_vars))]
                        subselect = np.logical_and.reduce(
                            criteria).nonzero()[0]
                    else:
                        subselect = np.arange(
                            ncf.get_variable('sounding_id').size)

                    # retrieval attributes
                    code = ncf.get_attribute('retrieval_id')
                    level_def = ncf.get_attribute('level_def')

                    # only read good quality observations
                    ids = ncf.get_variable('sounding_id').take(subselect,
                                                               axis=0)
                    lats = ncf.get_variable('latitude').take(subselect, axis=0)
                    lons = ncf.get_variable('longitude').take(subselect,
                                                              axis=0)
                    obs = ncf.get_variable('obs').take(subselect, axis=0)
                    unc = ncf.get_variable('uncertainty').take(subselect,
                                                               axis=0)
                    dates = ncf.get_variable('date').take(subselect, axis=0)
                    dates = array([dt.datetime(*d) for d in dates])
                    av_kernel = ncf.get_variable('averaging_kernel').take(
                        subselect, axis=0)
                    prior_profile = ncf.get_variable('prior_profile').take(
                        subselect, axis=0)
                    pressure = ncf.get_variable('pressure_levels').take(
                        subselect, axis=0)

                    prior = ncf.get_variable('prior').take(subselect, axis=0)

                    ##### freum vvvv
                    pwf = ncf.get_variable('pressure_weighting_function').take(
                        subselect, axis=0)

                    # Additional variable surface pressure in case the profiles are defined as layer averages
                    if level_def == "layer_average":
                        psurf = ncf.get_variable('surface_pressure').take(
                            subselect, axis=0)
                    else:
                        psurf = [float('nan')] * len(ids)

                    # Optional: footprint corners
                    latc = dict(latc_0=[float('nan')] * len(ids),
                                latc_1=[float('nan')] * len(ids),
                                latc_2=[float('nan')] * len(ids),
                                latc_3=[float('nan')] * len(ids))
                    lonc = dict(lonc_0=[float('nan')] * len(ids),
                                lonc_1=[float('nan')] * len(ids),
                                lonc_2=[float('nan')] * len(ids),
                                lonc_3=[float('nan')] * len(ids))
                    # If  one footprint corner variable  is there, assume
                    # all are there. That's the only case that makes sense
                    if 'latc_0' in list(ncf.variables.keys()):
                        latc['latc_0'] = ncf.get_variable('latc_0').take(
                            subselect, axis=0)
                        latc['latc_1'] = ncf.get_variable('latc_1').take(
                            subselect, axis=0)
                        latc['latc_2'] = ncf.get_variable('latc_2').take(
                            subselect, axis=0)
                        latc['latc_3'] = ncf.get_variable('latc_3').take(
                            subselect, axis=0)
                        lonc['lonc_0'] = ncf.get_variable('lonc_0').take(
                            subselect, axis=0)
                        lonc['lonc_1'] = ncf.get_variable('lonc_1').take(
                            subselect, axis=0)
                        lonc['lonc_2'] = ncf.get_variable('lonc_2').take(
                            subselect, axis=0)
                        lonc['lonc_3'] = ncf.get_variable('lonc_3').take(
                            subselect, axis=0)
                    ###### freum ^^^^

                    ncf.close()

                    # Add samples to datalist
                    # Note that the mdm is initialized here equal to the measurement uncertainty. This value is used in add_model_data_mismatch to calculate the mdm including model error
                    for n in range(len(ids)):
                        # Check for every sounding if time is between start and end time (relevant for first and last days of window)
                        if self.startdate <= dates[n] <= self.enddate:
                            self.datalist.append(TotalColumnSample(ids[n], code, dates[n], obs[n], None, lats[n], lons[n], unc[n], prior[n], prior_profile[n,:], \
                                                                    av_kernel=av_kernel[n,:], pressure=pressure[n,:], pressure_weighting_function=pwf[n,:],level_def=level_def,psurf=psurf[n],
                                                                    ##### freum vvvv
                                                                    latc_0=latc['latc_0'][n], latc_1=latc['latc_1'][n], latc_2=latc['latc_2'][n], latc_3=latc['latc_3'][n],
                                                                    lonc_0=lonc['lonc_0'][n], lonc_1=lonc['lonc_1'][n], lonc_2=lonc['lonc_2'][n], lonc_3=lonc['lonc_3'][n]
                                                                    ##### freum ^^^^
                                                                    ))

                    logging.debug("Added %d observations to the Data list" %
                                  (len(self.datalist) - len_init))

                ndays += 1

        del i

        if len(self.datalist) > 0:
            logging.info("Observations list now holds %d values" %
                         len(self.datalist))
        else:
            logging.info("No observations found for sampling window")

    def add_model_data_mismatch(self, filename=None, advance=False):
        """ This function is empty: model data mismatch calculation is done during sampling in observation operator (TM5) to enhance computational efficiency
            (i.e. to prevent reading all soundings twice and writing large additional files)

        """
        # obs_data = rc.read(self.obs_file)
        self.rejection_threshold = 15  #int(obs_data['obs.rejection.threshold'])

        # At this point mdm is set to the measurement uncertainty only, added in the add_observations function.
        # Here this value is used to set the combined mdm by adding an estimate for the model uncertainty as a sum of squares.
        if len(self.datalist) <= 1: return  #== 0: return
        for obs in self.datalist:
            obs.mdm = (
                obs.mdm * obs.mdm + 2**2
            )**0.5  ## Here changed into 2 (2ppm) for CO2 : ERIK, CHANGE THIS TO WHAT I NEED!
        del obs

        meanmdm = np.average(np.array([obs.mdm for obs in self.datalist]))
        logging.debug('Mean MDM = %s' % meanmdm)

    def add_simulations(self, filename, silent=False):
        """ Adds observed and model simulated column values to the mole fraction objects
            This function includes the add_observations and add_model_data_mismatch functionality for the sake of computational efficiency

        """

        if self.sample_in_ctdas:
            logging.debug(
                "CODE TO ADD SIMULATED SAMPLES TO DATALIST TO BE ADDED")

        else:
            # read simulated samples from file
            if not os.path.exists(filename):
                msg = "Sample output filename for observations could not be found : %s" % filename
                logging.error(msg)
                logging.error("Did the sampling step succeed?")
                logging.error("...exiting")
                raise IOError(msg)

            ncf = io.ct_read(filename, method='read')
            ids = ncf.get_variable('sounding_id')
            simulated = ncf.get_variable('column_modeled')
            ncf.close()
            logging.info("Successfully read data from model sample file (%s)" %
                         filename)

            obs_ids = self.getvalues('id').tolist()

            missing_samples = []

            # Match read simulated samples with observations in datalist
            logging.info("Adding %i simulated samples to the data list..." %
                         len(ids))
            for i in range(len(ids)):
                # Assume samples are in same order in both datalist and file with simulated samples...
                if ids[i] == obs_ids[i]:
                    self.datalist[i].simulated = simulated[i]
                # If not, find index of current sample
                elif ids[i] in obs_ids:
                    index = obs_ids.index(ids[i])
                    # Only add simulated value to datalist if sample has not been filled before. Otherwise: exiting
                    if self.datalist[index].simulated is not None:
                        msg = 'Simulated and observed samples not in same order, and duplicate sample IDs found.'
                        logging.error(msg)
                        raise IOError(msg)
                    else:
                        self.datalist[index].simulated = simulated[i]
                else:
                    logging.debug('added %s to missing_samples, obs id = %s' %
                                  (ids[i], obs_ids[i]))
                    missing_samples.append(ids[i])
            del i

            if not silent and missing_samples != []:
                logging.warning(
                    '%i Model samples were found that did not match any ID in the observation list. Skipping them...'
                    % len(missing_samples))

            # if number of simulated samples < observations: remove observations without samples
            if len(simulated) < len(self.datalist):
                test = len(self.datalist) - len(simulated)
                logging.warning(
                    '%i Observations were not sampled, removing them from datalist...'
                    % test)
                for index in reversed(list(range(len(self.datalist)))):
                    if self.datalist[index].simulated is None:
                        del self.datalist[index]
                del index

            logging.debug("%d simulated values were added to the data list" %
                          (len(ids) - len(missing_samples)))

    def write_sample_coords(self, obsinputfile):
        """
            Write empty sample_coords_file if soundings are present in time interval, just such that general pipeline code does not have to be changed...
        """

        if self.sample_in_ctdas:
            return

        if len(self.datalist) <= 1:  #== 0:
            logging.info(
                "No observations found for this time period, no obs file written"
            )
            return

        # write data required by observation operator for sampling to file
        f = io.CT_CDF(obsinputfile, method='create')
        logging.debug(
            'Creating new observations file for ObservationOperator (%s)' %
            obsinputfile)

        dimsoundings = f.add_dim('soundings', len(self.datalist))
        dimdate = f.add_dim('epoch_dimension', 7)
        dimchar = f.add_dim('char', 20)
        if len(self.datalist) == 1:
            dimlevels = f.add_dim('levels', len(self.getvalues('pressure')))
            # freum: inserted but commented Liesbeth's new code for layers for reference,
            # but I handle them differently.
            # if len(self.getvalues('av_kernel')) != len(self.getvalues('pressure')):
            #     dimlayers = f.add_dim('layers',len(self.getvalues('av_kernel')))
            #     layers = True
            # else: layers = False
        else:
            dimlevels = f.add_dim('levels',
                                  self.getvalues('pressure').shape[1])
            # if self.getvalues('av_kernel').shape[1] != self.getvalues('pressure').shape[1]:
            #     dimlayers = f.add_dim('layers', self.getvalues('pressure').shape[1] - 1)
            #     layers = True
            # else: layers = False

        savedict = io.std_savedict.copy()
        savedict['dtype'] = "int64"
        savedict['name'] = "sounding_id"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('id').tolist()
        f.add_data(savedict)

        data = [[
            d.year, d.month, d.day, d.hour, d.minute, d.second, d.microsecond
        ] for d in self.getvalues('xdate')]
        savedict = io.std_savedict.copy()
        savedict['dtype'] = "int"
        savedict['name'] = "date"
        savedict['dims'] = dimsoundings + dimdate
        savedict['values'] = data
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "latitude"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('lat').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "longitude"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('lon').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "prior"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('prior').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "prior_profile"
        savedict['dims'] = dimsoundings + dimlevels
        savedict['values'] = self.getvalues('prior_profile').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "averaging_kernel"
        savedict['dims'] = dimsoundings + dimlevels
        savedict['values'] = self.getvalues('av_kernel').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "pressure_levels"
        savedict['dims'] = dimsoundings + dimlevels
        savedict['values'] = self.getvalues('pressure').tolist()
        f.add_data(savedict)

        # freum vvvv
        savedict = io.std_savedict.copy()
        savedict['name'] = "pressure_weighting_function"
        savedict['dims'] = dimsoundings + dimlevels
        savedict['values'] = self.getvalues(
            'pressure_weighting_function').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "latc_0"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('latc_0').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "latc_1"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('latc_1').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "latc_2"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('latc_2').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "latc_3"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('latc_3').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "lonc_0"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('lonc_0').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "lonc_1"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('lonc_1').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "lonc_2"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('lonc_2').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "lonc_3"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('lonc_3').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "XCO2"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('obs').tolist()
        f.add_data(savedict)

        # freum ^^^^

        savedict = io.std_savedict.copy()
        savedict['dtype'] = "char"
        savedict['name'] = "level_def"
        savedict['dims'] = dimsoundings + dimchar
        savedict['values'] = self.getvalues('level_def').tolist()
        f.add_data(savedict)

        savedict = io.std_savedict.copy()
        savedict['name'] = "psurf"
        savedict['dims'] = dimsoundings
        savedict['values'] = self.getvalues('psurf').tolist()
        f.add_data(savedict)

        f.close()


################### End Class TotalColumnObservations ###################

if __name__ == "__main__":
    pass
