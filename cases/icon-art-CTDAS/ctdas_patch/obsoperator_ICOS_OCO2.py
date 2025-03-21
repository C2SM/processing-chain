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
# model.py
"""
.. module:: observationoperator
.. moduleauthor:: Wouter Peters 

Revision History:
File created on 30 Aug 2010.

"""

import logging
import subprocess
import datetime as dt
import numpy as np
from netCDF4 import Dataset
import os, sys
import subprocess
import time
import re
import da.tools.io4 as io

sys.path.append(os.getcwd())
sys.path.append('../../')

import da.tools.rc as rc
from da.tools.icon.icon_helper import ICON_Helper
from da.tools.icon.utilities import utilities
import subprocess
import glob

identifier = 'RandomizerObservationOperator'
version = '1.0'


################### Begin Class ObservationOperator ###################
class ObservationOperator(object):
    """
    Testing
    =======
    This is a class that defines an ObervationOperator. This object is used to control the sampling of
    a statevector in the ensemble Kalman filter framework. The methods of this class specify which (external) code
    is called to perform the sampling, and which files should be read for input and are written for output.

    The baseclasses consist mainly of empty methods that require an application specific application. The baseclass will take observed values, and perturb them with a random number chosen from the model-data mismatch distribution. This means no real operator will be at work, but random normally distributed residuals will come out of y-H(x) and thus the inverse model can proceed. This is mainly for testing the code...

    """

    def __init__(self, rc_filename, dacycle=None):
        """ The instance of an ObservationOperator is application dependent """
        self.ID = identifier
        self.version = version
        self.restart_filelist = []
        self.output_filelist = []
        self.outputdir = None  # Needed for opening the samples.nc files created

        # Load settings
        self._load_rc(rc_filename)
        self._validate_rc()

        # Instantiate an ICON_Helper object
        self.settings["dir.icon_sim"]
        self.iconhelper = ICON_Helper(self.settings)
        self.iconhelper.validate_settings(["dir.icon_sim"])

        logging.info('Observation Operator object initialized: %s (%s)',
                     self.ID, self.version)

        # The following code allows the object to be initialized with a dacycle object already present. Otherwise, it can
        # be added at a later moment.

        if dacycle != None:
            self.dacycle = dacycle
        else:
            self.dacycle = {{}}

    def _load_rc(self, name):
        """Read settings from the observation operator's rc-file

        Based on TM5ObservationOperator.load_rc
        """
        self.rcfile = rc.RcFile(name)
        self.settings = self.rcfile.values
        logging.debug(self.settings)
        self.rc_filename = name

        logging.debug("rc-file %s loaded", name)

    def _validate_rc(self):
        """Check that some required values are given in the rc-file.

        Based on TM5ObservationOperator.validate_rc
        """

        needed_rc_items = ["dir.icon_sim"]

        for key in needed_rc_items:
            if key not in self.settings:
                msg = "Missing a required value in rc-file : %s" % key
                logging.error(msg)
                raise IOError(msg)
        logging.debug("rc-file has been validated succesfully")

    def get_initial_data(self):
        """ This method places all initial data needed by an ObservationOperator in the proper folder for the model """

    def setup(self, dacycle):
        """ Perform all steps necessary to start the observation operator through a simple Run() call """

        self.dacycle = dacycle
        self.outputdir = dacycle['dir.output']
        self.simulationdir = self.settings["dir.icon_sim"]
        self.n_bg_params = int(dacycle['statevector.bg_params'])
        self.n_regs = int(dacycle['statevector.number_regions'])
        self.tracer = str(dacycle['statevector.tracer'])

    def prepare_run(self, samples):
        """ Prepare the running of the actual forecast model, for example compile code """

        import os

        # For each sample type, define the name of the file that will contain the modeled output of each observation
        self.simulated_file = [None] * len(samples)
        for i in range(len(samples)):
            self.simulated_file[i] = os.path.join(
                self.outputdir,
                '%s_output.%s.nc' % (samples[i].get_samples_type(),
                                     self.dacycle['time.sample.stamp']))
            logging.info("Simulated flask file added: %s" %
                         self.simulated_file[i])
        del i
        self.forecast_nmembers = int(self.dacycle['da.optimizer.nmembers'])

    def make_lambdas(self, statevector, lag):
        """ Write out lambda file parameters
        """
        #msteiner:
        #write lambda file for current lag:
        members = statevector.ensemble_members[lag]
        if statevector.isOptimized:
            self.lambda_file = os.path.join(
                self.simulationdir, 'global_inputs', 'OEM',
                'lambda_%s_opt2.nc' % self.dacycle['time.sample.stamp'][0:8])
            self.bg_lambda_file = os.path.join(
                self.simulationdir, 'global_inputs', 'OEM',
                'bg_lambda_%s_opt2.nc' %
                self.dacycle['time.sample.stamp'][0:8])
        else:
            if lag == 0:
                self.lambda_file = os.path.join(
                    self.simulationdir, 'global_inputs', 'OEM',
                    'lambda_%s_opt1.nc' %
                    self.dacycle['time.sample.stamp'][0:8])
                self.bg_lambda_file = os.path.join(
                    self.simulationdir, 'global_inputs', 'OEM',
                    'bg_lambda_%s_opt1.nc' %
                    self.dacycle['time.sample.stamp'][0:8])
            else:
                self.lambda_file = os.path.join(
                    self.simulationdir, 'global_inputs', 'OEM',
                    'lambda_%s_prior.nc' %
                    self.dacycle['time.sample.stamp'][0:8])
                self.bg_lambda_file = os.path.join(
                    self.simulationdir, 'global_inputs', 'OEM',
                    'bg_lambda_%s_prior.nc' %
                    self.dacycle['time.sample.stamp'][0:8])

        ofile = Dataset(self.lambda_file, mode='w')
        nr_ens = self.forecast_nmembers + 1 if {cfg.CTDAS_propagate_bg} else 0
        nr_reg = self.n_regs
        nr_cat = {max(lambdas)}
        nr_tracer = 1
        oens = ofile.createDimension('ens', nr_ens)
        oreg = ofile.createDimension('reg', nr_reg)
        ocat = ofile.createDimension('cat', nr_cat)
        otracer = ofile.createDimension('tracer', nr_tracer)
        odata = ofile.createVariable('lambda',
                                     np.float32,
                                     ('ens', 'reg', 'cat', 'tracer'),
                                     fill_value=-999.99)
        lambdas = np.empty(shape=(nr_ens, nr_reg, nr_cat, nr_tracer))
        for m in range(0, self.forecast_nmembers):
            param_count = 0
            for ireg in range(0, nr_reg):
                for icat in range(0, nr_cat):
                    if statevector.isOptimized:
                        lambdas[m, ireg, icat,
                                0] = members[0].param_values[param_count]
                    else:
                        lambdas[m, ireg, icat,
                                0] = members[m].param_values[param_count]
                    param_count += 1
        if {cfg.CTDAS_propagate_bg}:
            for ireg in range(0, nr_reg):
                for icat in range(0, nr_cat):
                    lambdas[-1, ireg, icat,
                            0] = 0.0  # Set anthropogenic component to 0
        odata[:] = lambdas
        ofile.close()
        logging.info('lambdas for ICON simulation written to the file: %s' %
                     self.lambda_file)

        #write bg_lambdas
        ofile = Dataset(self.bg_lambda_file, mode='w')
        nr_ens = self.forecast_nmembers + 1 if {cfg.CTDAS_propagate_bg} else 0
        nr_dir = {cfg.CTDAS_nboundaries}
        nr_tracer = 1
        oens = ofile.createDimension('ens', nr_ens)
        odir = ofile.createDimension('reg', nr_dir)
        # otracer = ofile.createDimension('tracer', nr_tracer)
        odata = ofile.createVariable('lambda',
                                     np.float32, ('ens', 'reg'),
                                     fill_value=-999.99)  #,'tracer'
        lambdas = np.empty(shape=(nr_ens, nr_dir))  #,nr_tracer
        for m in range(0, self.forecast_nmembers):
            for idir in range(0, nr_dir):
                if statevector.isOptimized:
                    lambdas[m,
                            idir] = members[0].param_values[-self.n_bg_params +
                                                            idir]
                else:
                    lambdas[m,
                            idir] = members[m].param_values[-self.n_bg_params +
                                                            idir]
        if {cfg.CTDAS_propagate_bg}:
            for idir in range(0, nr_dir):
                lambdas[-1, idir] = lambdas[
                    -2,
                    idir]  # Populate BG lambdas with the last member (which, for an optimized run, is the optimized member)
        odata[:] = lambdas
        ofile.close()
        logging.info('bg_lambdas for ICON simulation written to the file: %s' %
                     self.bg_lambda_file)

    def validate_input(self):
        """ Make sure that data needed for the ObservationOperator (such as observation input lists, or parameter files)
            are present.
        """

    def save_data(self):
        """ Write the data that is needed for a restart or recovery of the Observation Operator to the save directory """

    def run(self, samples, statevector, lag):
        """
         This Randomizer will take the original observation data in the Obs object, and simply copy each mean value. Next, the mean 
         value will be perturbed by a random normal number drawn from a specified uncertainty of +/- 2 ppm
        """

        import da.tools.io4 as io
        import numpy as np

        #select runscript for ICON-ART-OEM simulation:
        time = dt.datetime.strptime(self.dacycle['time.sample.stamp'][0:10],
                                    "%Y%m%d%H")
        job_timestr = f'{{time.strftime("%Y%m%d")}}'
        folder_timestr = f'{{time.strftime("%Y%m%d%H")}}_{{(time+dt.timedelta(days={cfg.CTDAS_ctdas_cycle})).strftime("%Y%m%d%H")}}'
        if statevector.isOptimized:
            runscript = os.path.join(
                self.simulationdir, folder_timestr, 'icon', 'run',
                '{ (cfg.case_path / cfg.icon_runjob_filename).stem }' +
                '_%s_opt2.job' % (self.dacycle['time.sample.stamp'][0:8]))
            self.outfolder = os.path.join('{cfg.case_root / "global_outputs"}',
                                          f"opt2_{{job_timestr}}")
        else:
            if lag == 0:
                runscript = os.path.join(
                    self.simulationdir, folder_timestr, 'icon', 'run',
                    '{ (cfg.case_path / cfg.icon_runjob_filename).stem }' +
                    '_%s_opt1.job' % (self.dacycle['time.sample.stamp'][0:8]))
                self.outfolder = os.path.join(
                    '{cfg.case_root / "global_outputs"}',
                    f"opt1_{{job_timestr}}")
            else:
                runscript = os.path.join(
                    self.simulationdir, folder_timestr, 'icon', 'run',
                    '{ (cfg.case_path / cfg.icon_runjob_filename).stem }' +
                    '_%s_prior.job' % (self.dacycle['time.sample.stamp'][0:8]))
                self.outfolder = os.path.join(
                    '{cfg.case_root / "global_outputs"}',
                    f"prior_{{job_timestr}}")

        logging.info('runscript name: %s' % (runscript))
        start_icon(runscript)
        logging.info('ICON done!')

    def sample(self, samples, statevector, lag):
        for j, sample in enumerate(samples):
            sample_type = sample.get_samples_type()
            logging.info(f"Want to do...{{sample_type}} extraction")
            if sample_type == "column":
                logging.info("Starting _launch_icon_column_sampling")

                warning_msg = "JM: Be careful! The current column sampling " + \
                              "method is designed for a specific case of study. " + \
                              "Please evaluate if the satellite product is suitable " + \
                              "with an appropriate model spatial resolution!"
                logging.warning(warning_msg)

                self._launch_icon_column_sampling(j, sample)

                logging.info("Finished _launch_icon_column_sampling")

            elif sample_type == "insitu":
                self.ICOS_sampling(j, sample, statevector, lag)

            else:
                logging.error("Unknown sample type: %s",
                              sample.get_samples_type())

    def ICOS_sampling(self, j, sample, statevector, lag):

        if statevector.isOptimized:
            prefix = 'opt2_'
        else:
            if lag == 0:
                prefix = 'prior_'
            else:
                prefix = 'opt1_'

        # Create a flask output file to hold simulated values for later reading
        f = io.CT_CDF(self.simulated_file[j], method='create')
        logging.debug(
            'Creating new simulated observation file in ObservationOperator (%s)'
            % self.simulated_file)

        dimid = f.createDimension('obs_num', size=None)
        dimid = ('obs_num', )
        savedict = io.std_savedict.copy()
        savedict['name'] = "obs_num"
        savedict['dtype'] = "int"
        savedict['long_name'] = "Unique_Dataset_observation_index_number"
        savedict['units'] = ""
        savedict['dims'] = dimid
        savedict[
            'comment'] = "Unique index number within this dataset ranging from 0 to UNLIMITED."
        f.add_data(savedict, nsets=0)

        dimmember = f.createDimension('nmembers', size=self.forecast_nmembers)
        dimmember = ('nmembers', )
        savedict = io.std_savedict.copy()
        savedict['name'] = "flask"
        savedict['dtype'] = "float"
        savedict['long_name'] = "mole_fraction_of_trace_gas_in_air"
        savedict['units'] = "mol tracer (mol air)^-1"
        savedict['dims'] = dimid + dimmember
        savedict[
            'comment'] = "Simulated model value created by RandomizerObservationOperator"
        f.add_data(savedict, nsets=0)

        # Open file with x,y,z,t of model samples that need to be sampled
        f_in = io.ct_read(self.dacycle['ObsOperator.inputfile.' +
                                       sample.get_samples_type()],
                          method='read')

        # Get simulated values and ID

        ids = f_in.get_variable('obs_num')
        obs = f_in.get_variable('observed')
        mdm = f_in.get_variable('modeldatamismatch')

        #msteiner:
        date_components = f_in.get_variable('date_components')
        evn = f_in.get_variable('evn')
        fromfile = f_in.get_variable('fromfile')
        #---------

        # Loop over observations, add random white noise, and write to file

        ###########################################################
        os.environ["HDF5_USE_FILE_LOCKING"] = "FALSE"

        molar_mass = {{'ch4': 16.04e-3, 'co2': 44.01e-3, 'da': 28.97e-3}}
        units_factor = {{
            'ch4': 1.e9,  #ppb for ch4
            'co2': 1.e6,  #ppm for co2                 
        }}

        import sys
        sys.path.insert(1, "{cfg.case_path / 'ICON'}")
        from Michael_sampler import ICON_sampler
        logging.info("Starting ICON sampling")
        # obs_lon, idx = np.unique(f_in.get_variable("longitude"), return_index=True)
        # obs_lat = f_in.get_variable("latitude")[idx]
        # inlet_height_agl = f_in.get_variable("inlet_height_over_base")[idx]
        # base_height_msl = f_in.get_variable("base_height_over_sea_level")[idx]
        # sampling_strategy = f_in.get_variable("sampling_strategy")
        # sampling_strategy = np.asarray([''.join(sampling_strategy[i].astype(str)) for i in range(sampling_strategy.shape[0])])
        # sampling_strategy_unique = sampling_strategy[idx]
        # unique_site_names = f_in.get_variable('evn')
        # unique_site_names = np.asarray([''.join(unique_site_names[i].astype(str)) for i in range(unique_site_names.shape[0])])
        # unique_site_names = unique_site_names[idx]
        time = dt.datetime.strptime(self.dacycle['time.sample.stamp'][0:10],
                                    "%Y%m%d%H")
        job_timestr = f'{{time.strftime("%Y%m%d")}}'
        starttime = f'{{time.strftime("%Y-%m-%d %H:%M:%S")}}'
        endtime = f"{{(time + dt.timedelta(seconds={cfg.CTDAS_restart_init_time}) + dt.timedelta(days={cfg.CTDAS_ctdas_cycle})).strftime('%Y-%m-%d %H:%M:%S')}}"
        obs_dir = os.path.join(self.simulationdir, "global_inputs", "ICOS")
        nneighb = 5
        meta = {meta_dict}
        meta["u"] = {{}}
        meta["v"] = {{}}
        meta["temp"] = {{}}
        meta["qv"] = {{}}
        outfile = os.path.join(self.simulationdir, "global_outputs",
                               "extracted_ICOS",
                               '%s%s.nc' % (prefix, job_timestr))
        # files = os.path.join(self.simulationdir,"global_outputs",'%s%s'%(prefix,job_timestr), 'ICON-ART-UNSTR*.nc')
        # logging.info(f"ICON files to sample: {{files}}")
        mountain_stations = {cfg.CTDAS["obs"]["ICOS"]["mountain_stations"]}
        mdm_dictionary = {
            evaluate_dict(
                {
                    k: v
                    for d in cfg.CTDAS["obs"]["ICOS"]["mdm"]
                    for k, v in d.items()
                }, "c_offset", cfg.CTDAS_obs_ICOS_c_offset)
        }  # Based on the simulated standard deviation of the signal (without background) over a full year.
        infolder = self.outfolder
        logging.info(f"Running ICON sampler with input folder {{infolder}}")
        logging.info(
            f"Running ICON sampler with starttime {{starttime}} and endtime {{endtime}} and obsdir {{obs_dir}} and nneighb {{nneighb}} and meta {{meta}} and outfile {{outfile}}"
        )
        ICON_sampler(infolder,
                     self.settings["output_prefix"],
                     "{cfg.input_files_scratch_dynamics_grid_filename}",
                     starttime,
                     endtime,
                     obs_dir,
                     nneighb,
                     meta,
                     outfile,
                     mountain_stations=mountain_stations)
        logging.info("Finished ICON sampling")
        logging.info(f"Written to output file {{outfile}}")

        simulated_values = np.zeros((len(obs), self.forecast_nmembers))
        f1 = io.ct_read(outfile, method='read')
        TR_A_ENS = (molar_mass['da'] / molar_mass[self.tracer]) * units_factor[
            self.tracer] * np.array(
                f1.get_variable('TR' + self.tracer.upper() + '_A_ENS') +
                f1.get_variable('biosource') - f1.get_variable('biosink')
            )  #float CH4_A_ENS(ens, sites, time) 1 --> ppb
        qv = np.array(f1.get_variable('qv'))  #float qv(sites, time)
        site_names = np.array(f1.get_variable('site_name'))
        obs_times = np.array(f1.get_variable('time'))

        # wet --> dry mmr
        for iiens in np.arange(self.forecast_nmembers):
            TR_A_ENS[iiens, ...] = TR_A_ENS[iiens, ...] / (1. - qv[...])

        #LOOP OVER OBS:
        for iobs in np.arange(len(obs)):
            station_name = fromfile[iobs][fromfile[iobs] !=
                                          b''].tostring().decode('utf-8')
            if station_name not in mdm_dictionary.keys():
                continue  # Skip stations that aren't considered
            print('DEBUG iobs: ', iobs, flush=True)
            obs_date = dt.datetime(*date_components[iobs, :])
            print('DEBUG obs_date: ', obs_date, flush=True)
            obs_date = obs_date.replace(minute=0, second=0)
            print('DEBUG modified obs_date: ', obs_date, flush=True)

            # LOOP OVER EXTRACTED DATA TIMES
            for itime in np.arange(TR_A_ENS.shape[2]):
                otime = dt.datetime.strptime(obs_times[itime], '%Y-%m-%dT%H')
                #                    print('DEBUG checking otime: ',otime,flush=True)
                if not (obs_date == otime): continue
                print('DEBUG found otime: ', otime, flush=True)

                # find index (or the difference) of hour at 12 UTC and 0 UTC
                if station_name in mountain_stations:
                    print('DEBUG station',
                          station_name,
                          'is a mountain site',
                          flush=True)
                    delta_index = obs_date.hour
                    print('DEBUG delta_index: ', delta_index, flush=True)
                else:
                    print('DEBUG station',
                          station_name,
                          'is NOT a mountain site',
                          flush=True)
                    delta_index = obs_date.hour - 12
                    print('DEBUG delta_index: ', delta_index, flush=True)

                # LOOP OVER STATIONS
                for isite in np.arange(TR_A_ENS.shape[1]):
                    site_name = site_names[isite]
                    #                        print('DEBUG looking through sampled stations. Checking site_name: ',site_name,flush=True)
                    if (site_name == station_name):
                        print(
                            'DEBUG looking through sampled stations. Found site_name: ',
                            site_name,
                            flush=True)
                        for iens in np.arange(self.forecast_nmembers):
                            if station_name in mountain_stations:
                                simulated_values[iobs, iens] = np.nanmean(
                                    TR_A_ENS[iens, isite,
                                             itime - delta_index:itime -
                                             delta_index + 7])
                            else:
                                simulated_values[iobs, iens] = np.nanmean(
                                    TR_A_ENS[iens, isite,
                                             itime - delta_index:itime -
                                             delta_index + 5])
                            if iens == 50:
                                print(
                                    'Added model value for member 0 of %.2f for iobs %i at %s at %s with a delta idx of %i'
                                    % (simulated_values[iobs, 0], iobs,
                                       site_name, obs_date, delta_index))
                                print(
                                    'Added model value for member 50 of %.2f for iobs %i at %s at %s with a delta idx of %i'
                                    % (simulated_values[iobs, 50], iobs,
                                       site_name, obs_date, delta_index))
                        break
                    else:
                        continue
                break
###########################################################

        for i in range(0, len(obs)):
            f.variables['obs_num'][i] = ids[i]
            f.variables['flask'][i, :] = simulated_values[i]

        f.close()
        f_in.close()

        # Report success and exit
        logging.info(
            'ICOS ObservationOperator finished successfully, output file written (%s)'
            % self.simulated_file)

    def _launch_icon_column_sampling(self, j, sample):
        """Sample ICON output at coordinates of column observations."""
        """Here we can implement Erik's CDO technique."""

        # To be continued....
        # run_dir = self.settings["dir.icon_sim"] # Erik: run_dir here means: output dir.
        # run_dir = '/scratch/snx3000/ekoene/processing-chain/work/VPRM_EU_ERA5_22/XCO2_test' # This should, eventually, be determined automatically from however the folder structure is made!
        run_dir = os.path.join(self.outfolder)
        logging.info(
            f"Directory that satellite data will be taken from: {{run_dir}}")

        sampling_coords_file = self.dacycle['ObsOperator.inputfile.' +
                                            sample.get_samples_type()]
        logging.info(f"Sampling coords file: {{sampling_coords_file}}")

        # Reconstruct self.simulated_file[i]
        out_file = self.simulated_file[j]
        nprocs = 1
        Nobs = len(sample.datalist)
        if Nobs == 0:
            logging.info("No observations, skipping sampling")
            return

        # Make run command
        command_ = " "  # Erik: this would have to look different for us
        # Submit processes
        procs = list()
        for nproc in range(nprocs):
            cmd = " ".join([
                command_, "python ./da/tools/icon/icon_sampler.py",
                "--nproc %d" % nproc,
                "--nprocs %d" % nprocs,
                "--sampling_coords_file %s" % sampling_coords_file,
                "--run_dir %s" % run_dir,
                "--iconout_prefix %s" % self.settings["output_prefix"],
                "--icon_grid %s" % self.settings["icon_grid_path"],
                "--nmembers %d" % int(self.forecast_nmembers),
                "--tracer_optim %s" % self.settings["tracer_optim"],
                "--outfile_prefix %s" % out_file,
                "--footprint_samples_dim %d" %
                int(self.settings['obs.column.footprint_samples_dim'])
            ])

            procs.append(
                subprocess.Popen(cmd.split(),
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.STDOUT))

        logging.info("Started %d sampling process(es).", nprocs)
        logging.debug("Command of last process: %s", cmd)

        # Wait for all processes to finish
        for n in range(nprocs):
            procs[n].wait()

        # Check for errors
        retcodes = []
        for n in range(nprocs):
            logging.debug("Checking errors in process %d", n)
            retcodes.append(utilities.check_out_err(procs[n]))

        if any([r != 0 for r in retcodes]):
            raise RuntimeError("At least one sampling process " + \
                                "finished with errors.")

        logging.info("All sampling processes finished.")

        # Join output files
        logging.info("Joining output files.")

        # Finishing msg
        logging.info("ICON column output sampled.")
        logging.info("If samples object carried observations, output " + \
                     "file written to %s", self.simulated_file)

########################################################################################

    def run_forecast_model(self, samples, statevector, lag, dacycle):
        self.startdate = dacycle['time.sample.start']
        self.prepare_run(samples)
        self.make_lambdas(statevector, lag)
        self.validate_input()
        self.run(samples, statevector, lag)
        self.sample(samples, statevector, lag)
        self.save_data()


################### End Class ObservationOperator ###################


class RandomizerObservationOperator(ObservationOperator):
    """ This class holds methods and variables that are needed to use a random number generated as substitute
        for a true observation operator. It takes observations and returns values for each obs, with a specified 
        amount of white noise added 
    """


def wait_for_job(job_id):
    """Wait for a job to complete and check if all states are COMPLETED."""
    if not job_id:
        return False, "UNKNOWN"

    while True:
        result = subprocess.run(f"sacct -j {job_id} --format=State --noheader",
                                shell=True,
                                capture_output=True,
                                text=True)

        # Extract all job states from the output
        states = [s.strip() for s in result.stdout.split("\n") if s.strip()]
        logging.info(f"Job {job_id} finished with states: {states}")

        if states:
            if all(s == "COMPLETED" for s in states):
                return True, "COMPLETED"
            elif any(s in ["FAILED", "CANCELLED", "TIMEOUT"] for s in states):
                return False, "FAILED"

        time.sleep(10)


def submit_job(command):
    """Submit a job and return the job ID."""
    logging.info(f"Submitting job: {command}")
    result = subprocess.run(command,
                            shell=True,
                            capture_output=True,
                            text=True,
                            check=False)
    match = re.search(r"Submitted batch job (\d+)", result.stdout)

    if match:
        return match.group(1)

    logging.error("Failed to get job ID from sbatch output.")
    return None


def start_icon(runscript, max_retries=3):
    """Start an ICON job, retrying if it fails."""
    retries = 0

    while retries < max_retries:
        command = f"uenv run icon-wcp -- sbatch {runscript} --wait"
        logging.info(f"Starting ICON case job: {command}")
        job_id = submit_job(command)

        if not job_id:
            logging.error("Failed to submit job.")
            return False  # Failed to even submit

        logging.info(f"Running job ID {job_id}")
        completed, state = wait_for_job(job_id)

        if completed:
            return True  # Job finished successfully

        # Job failed, retry if under max_retries
        retries += 1
        logging.warning(
            f"Job failed with state {state}. Retrying {retries}/{max_retries}..."
        )

    logging.error(f"Job failed after {max_retries} retries.")
    return False  # Exhausted all retries


if __name__ == "__main__":
    pass
