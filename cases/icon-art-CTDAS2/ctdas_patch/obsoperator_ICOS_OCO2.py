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
from multiprocessing import Pool
from scipy import interpolate
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

    def __init__(self,
                 rc_filename,
                 dacycle=None):  # David: addition arg "rc_filename" added.
        """ The instance of an ObservationOperator is application dependent """
        self.ID = identifier
        self.version = version
        self.restart_filelist = []
        self.output_filelist = []
        self.outputdir = None  # Needed for opening the samples.nc files created

        # vvv Added by David:
        # Load settings
        self._load_rc(rc_filename)
        self._validate_rc()

        # Instantiate an ICON_Helper object *David could be useful for icon sampler
        self.settings["dir.icon_sim"]

        self.iconhelper = ICON_Helper(self.settings)
        self.iconhelper.validate_settings(["dir.icon_sim"])
        # ^^^ Added by David: ^^^

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
        self.rc_filename = name

        logging.debug("rc-file %s loaded", name)

    def _validate_rc(self):
        """Check that some required values are given in the rc-file.

        Based on TM5ObservationOperator.validate_rc
        """

        needed_rc_items = ["dir.icon_sim", "obsoperator.icon_exe"]

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
        self.simulationdir = dacycle['dir.icon_sim']
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
        #self.simulated_file = os.path.join(self.outputdir, 'samples_simulated.%s.nc' % self.dacycle['time.sample.stamp'])
        self.forecast_nmembers = int(self.dacycle['da.optimizer.nmembers'])

    def make_lambdas(self, statevector, lag):
        """ Write out lambda file parameters
        """
        #msteiner:
        #write lambda file for current lag:
        members = statevector.ensemble_members[lag]
        if statevector.isOptimized:
            self.lambda_file = os.path.join(
                self.simulationdir, 'input', 'oae',
                'lambda_%s_opt.nc' % self.dacycle['time.sample.stamp'][0:10])
            self.bg_lambda_file = os.path.join(
                self.simulationdir, 'input', 'oae', 'bg_lambda_%s_opt.nc' %
                self.dacycle['time.sample.stamp'][0:10])
        else:
            if lag == 0:
                self.lambda_file = os.path.join(
                    self.simulationdir, 'input', 'oae',
                    'lambda_%s_priorcycle1.nc' %
                    self.dacycle['time.sample.stamp'][0:10])
                self.bg_lambda_file = os.path.join(
                    self.simulationdir, 'input', 'oae',
                    'bg_lambda_%s_priorcycle1.nc' %
                    self.dacycle['time.sample.stamp'][0:10])
            else:
                self.lambda_file = os.path.join(
                    self.simulationdir, 'input', 'oae', 'lambda_%s_prior.nc' %
                    self.dacycle['time.sample.stamp'][0:10])
                self.bg_lambda_file = os.path.join(
                    self.simulationdir, 'input', 'oae',
                    'bg_lambda_%s_prior.nc' %
                    self.dacycle['time.sample.stamp'][0:10])

        # if os.path.exists(self.lambda_file):
        #     os.system('mv %s %s_cycle1.nc'%(self.lambda_file,self.lambda_file[:-3]))
        # if os.path.exists(self.bg_lambda_file):
        #     os.system('mv %s %s_cycle1.nc'%(self.lambda_file,self.lambda_file[:-3]))

        ofile = Dataset(self.lambda_file, mode='w')
        nr_ens = self.forecast_nmembers
        nr_reg = self.n_regs
        nr_cat = 2
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
        odata[:] = lambdas
        ofile.close()
        logging.info('lambdas for ICON simulation written to the file: %s' %
                     self.lambda_file)

        #write bg_lambdas
        ofile = Dataset(self.bg_lambda_file, mode='w')
        nr_ens = self.forecast_nmembers
        nr_dir = 8
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

        #msteiner:
        #write lambda file for current lag:
        members = statevector.ensemble_members[lag]
        if statevector.isOptimized:
            self.lambda_file = os.path.join(
                self.simulationdir, 'input', 'oae',
                'lambda_%s_opt.nc' % self.dacycle['time.sample.stamp'][0:10])
            self.bg_lambda_file = os.path.join(
                self.simulationdir, 'input', 'oae', 'bg_lambda_%s_opt.nc' %
                self.dacycle['time.sample.stamp'][0:10])
        else:
            if lag == 0:
                self.lambda_file = os.path.join(
                    self.simulationdir, 'input', 'oae',
                    'lambda_%s_priorcycle1.nc' %
                    self.dacycle['time.sample.stamp'][0:10])
                self.bg_lambda_file = os.path.join(
                    self.simulationdir, 'input', 'oae',
                    'bg_lambda_%s_priorcycle1.nc' %
                    self.dacycle['time.sample.stamp'][0:10])
            else:
                self.lambda_file = os.path.join(
                    self.simulationdir, 'input', 'oae', 'lambda_%s_prior.nc' %
                    self.dacycle['time.sample.stamp'][0:10])
                self.bg_lambda_file = os.path.join(
                    self.simulationdir, 'input', 'oae',
                    'bg_lambda_%s_prior.nc' %
                    self.dacycle['time.sample.stamp'][0:10])

        # if os.path.exists(self.lambda_file):
        #     os.system('mv %s %s_cycle1.nc'%(self.lambda_file,self.lambda_file[:-3]))
        # if os.path.exists(self.bg_lambda_file):
        #     os.system('mv %s %s_cycle1.nc'%(self.lambda_file,self.lambda_file[:-3]))

        ofile = Dataset(self.lambda_file, mode='w')
        nr_ens = self.forecast_nmembers
        nr_reg = self.n_regs
        nr_cat = 2
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
        odata[:] = lambdas
        ofile.close()
        logging.info('lambdas for ICON simulation written to the file: %s' %
                     self.lambda_file)

        #write bg_lambdas
        ofile = Dataset(self.bg_lambda_file, mode='w')
        nr_ens = self.forecast_nmembers
        nr_dir = 8
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
        odata[:] = lambdas
        ofile.close()
        logging.info('bg_lambdas for ICON simulation written to the file: %s' %
                     self.bg_lambda_file)

        #msteiner:
        #select runscript for ICON-ART-OEM simulation:
        if statevector.isOptimized:
            #icon_path = os.path.join(self.simulationdir,'output_%s_opt'%(self.dacycle['time.sample.stamp'][0:10]))
            runscript = os.path.join(
                self.simulationdir, 'run',
                'runscript_%sopt' % (self.dacycle['time.sample.stamp'][0:10]))
            #runscript_boundaries = os.path.join(self.simulationdir,'run','runscript_%sopt'%(self.dacycle['time.sample.stamp'][0:10]))
            extraction_script = os.path.join(
                self.simulationdir, 'run',
                'extract_%sopt' % (self.dacycle['time.sample.stamp'][0:10]))
            #extraction_script_boundaries = os.path.join(self.simulationdir,'run','extract_boundaries%sopt'%(self.dacycle['time.sample.stamp'][0:10]))
            extracted_file = os.path.join(
                self.simulationdir, 'extracted',
                'output_%s_opt' % (self.dacycle['time.sample.stamp'][0:10]))
        else:
            if lag == 0:
                runscript = os.path.join(
                    self.simulationdir, 'run', 'runscript_%spriorcycle1' %
                    (self.dacycle['time.sample.stamp'][0:10]))
                extraction_script = os.path.join(
                    self.simulationdir, 'run', 'extract_%spriorcycle1' %
                    (self.dacycle['time.sample.stamp'][0:10]))
                extracted_file = os.path.join(
                    self.simulationdir, 'extracted', 'output_%s_priorcycle1' %
                    (self.dacycle['time.sample.stamp'][0:10]))
            else:
                runscript = os.path.join(
                    self.simulationdir, 'run', 'runscript_%sprior' %
                    (self.dacycle['time.sample.stamp'][0:10]))
                extraction_script = os.path.join(
                    self.simulationdir, 'run', 'extract_%sprior' %
                    (self.dacycle['time.sample.stamp'][0:10]))
                #extraction_script_boundaries = os.path.join(self.simulationdir,'run','extract_boundaries%sprior'%(self.dacycle['time.sample.stamp'][0:10]))
                #icon_path = os.path.join(self.simulationdir,'output_%s_prior'%(self.dacycle['time.sample.stamp'][0:10]))
                extracted_file = os.path.join(
                    self.simulationdir, 'extracted', 'output_%s_prior' %
                    (self.dacycle['time.sample.stamp'][0:10]))
        runscript_boundaries = os.path.join(
            self.simulationdir, 'run_bg', 'runscript_boundaries%spriorcycle1' %
            (self.dacycle['time.sample.stamp'][0:10]))
        extraction_script_boundaries = os.path.join(
            self.simulationdir, 'run_bg', 'extract_boundaries%spriorcycle1' %
            (self.dacycle['time.sample.stamp'][0:10]))
        extracted_boundaries_ens_file = os.path.join(
            self.simulationdir, 'extracted', 'output_bg_%s_priorcycle1' %
            (self.dacycle['time.sample.stamp'][0:10]))
        logging.info('extraction_script: %s' % (extraction_script))

        template = os.path.join(self.simulationdir, 'run', 'templates',
                                'sbatch_extract_template')
        sbatch_script = os.path.join(self.simulationdir, 'run',
                                     'sbatch_script')
        sbatch_script_bg = os.path.join(self.simulationdir, 'run_bg',
                                        'sbatch_script')
        # Write sbatch file
        with open(template) as input_file:
            to_write = input_file.read()
        with open(sbatch_script, "w") as outf:
            outf.write(to_write.format(extract_script=extraction_script))

        self.extracted_file = extracted_file
        # inidata = os.path.join(
        #         self.simulationdir,
        #         'input',
        #         'icbc',
        #         self.startdate.strftime(cfg.meteo_nameformat) + '.nc')
        # link = os.path.join(
        #         '/users/nponomar/Emissions/ART', #ART input folder same as specified in ICON nml
        #         'ART_ICE_iconR19B09-grid_.nc' #ini5 from processing chain
        #         )
        # os.system('ln -sf ' + inidata + ' ' + link)

        #now run ICON-ART-OEM:
        # if not (os.path.exists(extracted_file) or os.path.exists(extracted_boundaries_ens_file)):
        #         logging.info('In branch 0')
        #         self.start_multiple_icon_jobs([runscript, runscript_boundaries])
        #         logging.info('ICON ensemble and boudnaries runs - done!')
        #         with open(sbatch_script_bg, "w") as outf:
        #          outf.write(to_write.format(extract_script=extraction_script_boundaries))
        #         #self.start_icon(sbatch_script_bg)
        #         self.start_multiple_icon_jobs([sbatch_script, sbatch_script_bg])
        #         logging.info('Extraction for ensemble and boudnaries runs - done!')
        while not (os.path.exists(extracted_file)):
            logging.info('In branch 1')
            logging.info('runscript name: %s' % (runscript))
            self.start_icon(runscript)
            logging.info('ICON done!')
            #now run the extraction script:
            self.start_icon(sbatch_script)
            logging.info('extractionscript name: %s' % (sbatch_script))
            logging.info('Extraction done!')
        # if not (os.path.exists(extracted_boundaries_ens_file)):
        #         logging.info('In branch 2')
        #         logging.info('runscript name: %s'%(runscript_boundaries))
        #         self.start_icon(runscript_boundaries)
        #         logging.info('ICON boundaries done!')
        #         with open(sbatch_script_bg, "w") as outf:
        #          outf.write(to_write.format(extract_script=extraction_script_boundaries))
        #         self.start_icon(sbatch_script_bg)
        #         logging.info('runscript name: %s'%(sbatch_script_bg))
        #         logging.info('Extraction done!')

    def sample(self, samples):
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
                self.ICOS_sampling(j, sample)

            else:
                logging.error("Unknown sample type: %s",
                              sample.get_samples_type())

    def ICOS_sampling(self, j, sample):
        # logging.info('WARNING!! Just for testing, Im copying the input file to the output file!')

        # cmd = f"cp {{self.dacycle['ObsOperator.inputfile.'+sample.get_samples_type()]}} {{self.simulated_file[j]}}"
        # logging.info(f"Will run cmd={{cmd}}")
        # os.system(cmd)
        # cmd = f"module load daint-mc NCO; ncrename -v observed,flask {{self.simulated_file[j]}}"
        # logging.info(f"Will run cmd={{cmd}}")
        # os.system(cmd)
        # return

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

        #M_CH4 = 16.04e-3 #mol. weight CH4 [kg/mol]
        #M_da = 28.97e-3 #mol. weight dry air [kg/mol]

        #mountain_sites = ['cmn_insitu','jfj_insitu','kas_insitu','oxk_icos','oxk_ingos','oxk_noaa','pdm_lsceflask','puy_insitu','puy_lsceflask','zsf_wdcgg','cur_wdcgg','pdm_lsce','snb_wdcgg']
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

        simulated_values = np.zeros((len(obs), self.forecast_nmembers))

        f1 = io.ct_read(self.extracted_file, method='read')
        TR_A_ENS = (molar_mass['da'] / molar_mass[self.tracer]) * units_factor[
            self.tracer] * np.array(
                f1.get_variable('TR' + self.tracer.upper() + '_A_ENS') +
                f1.get_variable('biosource_all_chemtr') -
                f1.get_variable('biosink_chemtr')
            )  #float CH4_A_ENS(ens, sites, time) 1 --> ppb
        qv = np.array(f1.get_variable('qv'))  #float qv(sites, time)
        site_names = np.array(f1.get_variable('site_name'))
        obs_times = np.array(f1.get_variable('time'))

        # wet --> dry mmr
        for iiens in np.arange(TR_A_ENS.shape[0]):
            TR_A_ENS[iiens, ...] = TR_A_ENS[iiens, ...] / (1. - qv[...])

        #LOOP OVER OBS:
        for iobs in np.arange(len(obs)):
            station_name = fromfile[iobs][fromfile[iobs] !=
                                          b''].tostring().decode('utf-8')
            if station_name in skip_stations:
                continue  # Skip stations outside of the domain!
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
        run_dir = os.path.join(self.simulationdir,
                               os.path.basename(self.extracted_file))
        logging.info(
            f"Directory that satellite data will be taken from: {{run_dir}}")

        sampling_coords_file = self.dacycle['ObsOperator.inputfile.' +
                                            sample.get_samples_type()]
        logging.info(f"Sampling coords file: {{sampling_coords_file}}")

        # Reconstruct self.simulated_file[i]
        out_file = self.simulated_file[j]
        # out_file = self._sim_fpattern % sample.get_samples_type()

        # Remove intermediate files from a previous sampling job (might
        # still be there if that one fails)
        # The file pattern is hardcoded in wrfout_sampler
        # slicefile_pattern = out_file + ".*.slice"
        # for f in  glob.glob(os.path.join(run_dir, slicefile_pattern)):
        #     os.remove(f)

        # Sould be parallelized?
        # Spawn multiple icon_sampler instances,
        # using at most all processes available
        #nprocs1 = int(self.dacycle["da.resources.ntasks"])
        nprocs1 = int(1)

        # Might not want to use that many processes if there are few
        # observations, because of overhead. Set a minimum number of
        # observations per process, and reduce the number of
        # processes to hit that.
        Nobs = len(sample.datalist)
        if Nobs == 0:
            logging.info("No observations, skipping sampling")
            return

        # Might want to increase this, no idea if this is reasonable
        nobs_min = 100
        nprocs2 = max(1, int(float(Nobs) / float(nobs_min)))

        # Number of processes to use:
        nprocs = min(nprocs1, nprocs2)

        # Make run command
        # For a task with 1 processor, specifically request -N1 because
        # otherwise slurm apparently sometimes tries to allocate one task to
        # more than one node. Or something like that. See here:
        # https://stackoverflow.com/questions/24056961/running-slurm-script-with-multiple-nodes-launch-job-steps-with-1-task
        #command_ = "srun --exclusive -n1 -N1"
        command_ = " "  # Erik: this would have to look different for us

        # Check if output slice files are already present
        # This shouldn't happen, because they are deleted
        # a few lines above. But if for some reason (crash)
        # they are still here, this might lead to funny behavios.
        if nprocs > 1:
            output_files = glob.glob(slicefile_pattern)
            if len(output_files) > 0:
                msg = "Files that match the pattern of the " + \
                      "sampler output are already present. Stopping."
                logging.error(msg)
                raise OSError(msg)

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
                "--nmembers %d" % int(self.dacycle["da.optimizer.nmembers"]),
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
        ### Some code for joining the files
        if nprocs > 1:
            utilities.cat_ncfiles(run_dir,
                                  slicefile_pattern,
                                  "sounding_id",
                                  out_file,
                                  in_pattern=True)

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
        self.sample(samples)
        self.save_data()

    def start_icon(self, runscript):
        os.system('sbatch --wait ' + runscript)
#        pass

    def start_multiple_icon_jobs(self, scripts):
        files = scripts
        #command = "sbatch --wait "
        os.system('sbatch  ' + files[1])
        os.system('sbatch  --wait ' + files[0])
        # processes = list()
        # max_processes = len(files)

        # for name in files:
        #      logging.info('Starting a new job: %s'%(command + name))
        #      processes.append(subprocess.Popen([command + name], shell=True))

        #     # if len(processes) >= max_processes:
        # os.wait()
        # processes.difference_update([
        #      p for p in processes if p.poll() is not None])


################### End Class ObservationOperator ###################


class RandomizerObservationOperator(ObservationOperator):
    """ This class holds methods and variables that are needed to use a random number generated as substitute
        for a true observation operator. It takes observations and returns values for each obs, with a specified 
        amount of white noise added 
    """


if __name__ == "__main__":
    pass
