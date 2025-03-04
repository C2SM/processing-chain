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
# da_initexit.py

"""
.. module:: initexit
.. moduleauthor:: Wouter Peters 

Revision History:
File created on 13 May 2009.

The CycleControl class is found in the module :mod:`initexit`. It is derived from the standard python :class:`dictionary` object. It is the only core object of CTDAS that is automatically created in the pipeline, the user (normally) does not need to modify or extend it. The class is created based on options and arguments passes on the command line when submitting your main CTDAS job. 

Valid options are defined in 

.. autofunction:: da.tools.initexit.parse_options

With the name of a valid ``rc-file``, the CycleControl object is instantiated and validated. An example rc-file looks
like this:::

    ! Info on the data assimilation cycle

    time.restart        : False                     ! Restart from an existing run T/F
    time.start          : 2000-01-01 00:00:00       ! Start time of first cycle
    time.finish         : 2000-01-08 00:00:00       ! End time of last cycle
    time.cycle          : 7                         ! length of each cycle, 7 means one week
    time.nlag           : 5                         ! number of cycles in one smoother window
    dir.da_run          : ${{HOME}}/tmp/test_da       ! the run directory for you project

    ! Info on the DA system used

    da.system           : CarbonTracker             ! an identifier for your inversion system
    da.system.rc        : da/rc/carbontracker.rc    ! the settings needed in your inversion system

    ! Info on the forward model to be used

    da.obsoperator         : TM5                                ! an identifier for your observation operator
    da.obsoperator.rc      : ${{HOME}}/Modeling/TM5/tm5-ctdas.rc  ! the rc-file needed to run youobservation operator
    da.optimizer.nmembers  : 30                                 ! the number of ensemble members desired in the optimization

The most important method of the CycleControl object are listed below:

.. autoclass:: da.tools.initexit.CycleControl 
   :members: setup, finalize,  collect_restart_data, move_restart_data, 
             submit_next_cycle, setup_file_structure, recover_run, random_seed

Two important attributes of the CycleControl object are:
    (1) DaSystem, an instance of a :ref:`dasystem`
    (2) DaPlatForm, an instance of a :ref:`platform`

Other functions in the module initexit that are related to the control of a DA cycle are:

.. autofunction:: da.tools.initexit.start_logger 
.. autofunction:: da.tools.initexit.validate_opts_args 


"""
import logging
import os
import sys
import glob
import shutil
import copy
import getopt
import pickle
import numpy as np
#from string import join

import da.tools.rc as rc
from da.tools.general import create_dirs, to_datetime, advance_time

needed_da_items = [
    'time.start',
    'time.finish',
    'time.nlag',
    'time.cycle',
    'dir.da_run',
    'da.resources.ncycles_per_job',
    'da.resources.ntasks',
    'da.resources.ntime',
    'da.system',
    'da.system.rc',
    'da.obsoperator',
    'da.optimizer.nmembers']

# only needed in an earlier implemented where each substep was a separate job
# validprocesses = ['start','done','samplestate','advance','invert']


class CycleControl(dict):
    """
    This object controls the CTDAS system flow and functionality.
    """
        
    def __init__(self, opts=[], args={{}}):
        """
        The CycleControl object is instantiated with a set of options and arguments.
        The list of arguments must contain the name of an existing ``rc-file``. 
        This rc-file is loaded by method :meth:`~da.tools.initexit.CycleControl.load_rc` and validated
        by :meth:`~da.tools.initexit.CycleControl.validate_rc`

        Options for the CycleControl consist of accepted command line flags or arguments 
        in :func:`~da.tools.initexit.CycleControl.parse_options`

        """
        rcfile = args['rc']
        self.load_rc(rcfile)
        self.validate_rc()
        self.opts = opts

        # Add some useful variables to the rc-file dictionary

        self['jobrcfilename'] = rcfile
        self['dir.da_submit'] = os.getcwd()
        self['da.crash.recover'] = '-r' in opts
        self['transition'] = '-t' in opts
        self['verbose'] = '-v' in opts
        self.dasystem = None # to be filled later
        self.restart_filelist = [] # List of files needed for restart, to be extended later
        self.output_filelist = [] # List of files needed for output, to be extended later


    def load_rc(self, rcfilename):
        """ 
        This method loads a DA Cycle rc-file with settings for this simulation 
        """

        rcdata = rc.read(rcfilename)
        for k, v in list(rcdata.items()):
            self[k] = v

        logging.info('DA Cycle rc-file (%s) loaded successfully' % rcfilename)
        

    def validate_rc(self):
        """ 
        Validate the contents of the rc-file given a dictionary of required keys. 
        Currently required keys are :attr:`~da.tools.initexit.needed_da_items`
        """

        for k, v in list(self.items()):
            if v in ['True', 'true', 't', 'T', 'y', 'yes']:
                self[k] = True
            if v in ['False', 'false', 'f', 'F', 'n', 'no']:
                self[k] = False
            if 'date' in k : 
                self[k] = to_datetime(v)
            if k in ['time.start', 'time.end', 'time.finish', 'da.restart.tstamp']:
                self[k] = to_datetime(v)
        for key in needed_da_items:
            if key not in self:
                msg = 'Missing a required value in rc-file : %s' % key
                logging.error(msg)
                logging.error('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ')
                logging.error('Please note the update on Dec 02 2011 where rc-file names for DaSystem and ')
                logging.error('are from now on specified in the main rc-file (see da/rc/da.rc for example)')
                logging.error('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ')
                raise IOError(msg)
        logging.debug('DA Cycle settings have been validated succesfully')

    def parse_times(self):
        """ 
        Parse time related parameters into datetime objects for later use 
        """

        startdate = self['time.start']
        finaldate = self['time.finish']                  

        if finaldate <= startdate:
            logging.error('The start date (%s) is not greater than the end date (%s), please revise' % (startdate.strftime('%Y%m%d'), finaldate.strftime('%Y%m%d'))) 
            raise ValueError
        cyclelength = self['time.cycle']                 # get time step

# Determine end date

        if cyclelength == 'infinite':
            enddate = finaldate
        else:
            enddate = advance_time(startdate, cyclelength)

        dt = enddate - startdate

        if enddate > finaldate:  # do not run beyond finaldate
            enddate = finaldate

        self['time.start'] = startdate
        self['time.end'] = enddate
        self['time.finish'] = finaldate
        self['cyclelength'] = dt

        logging.info("===============================================================")
        logging.info("DA Cycle start date is %s" % startdate.strftime('%Y-%m-%d %H:%M'))
        logging.info("DA Cycle end date is %s" % enddate.strftime('%Y-%m-%d %H:%M'))
        logging.info("DA Cycle final date is %s" % finaldate.strftime('%Y-%m-%d %H:%M'))  
        logging.info("DA Cycle cycle length is %s" % cyclelength)
        logging.info("DA Cycle restart is %s" % str(self['time.restart']))
        logging.info("===============================================================")


    def set_sample_times(self, lag):
        """
        Set the times over which a sampling interval will loop, depending on 
        the lag. Note that lag falls in the interval [0,nlag-1]
        """

        # Start from cycle times 
        self['time.sample.start'] = copy.deepcopy(self['time.start'])
        self['time.sample.end'] = copy.deepcopy(self['time.end'])

        # Now advance depending on lag

        for l in range(lag):
            self.advance_sample_times()


    def advance_sample_times(self):
        """ 
        Advance sampling start and end time by one cycle interval
        """

        days = self['cyclelength'].days                

        self['time.sample.start'] = advance_time(self['time.sample.start'], days)
        self['time.sample.end'] = advance_time(self['time.sample.end'], days)
    

    def advance_cycle_times(self):
        """ 
        Advance cycle start and end time by one cycle interval
        """
              
        days = self['cyclelength'].days                  

        startdate = advance_time(self['time.start'], days)
        enddate = advance_time(self['time.end'], days)

        filtertime = startdate.strftime('%Y%m%d')
        self['dir.output'] = os.path.join(self['dir.da_run'], 'output', filtertime)

        self['time.start'] = startdate
        self['time.end'] = enddate


    def write_random_seed(self):
        filename = os.path.join(self['dir.restart'], 'randomseed_%s.pickle' % self['time.start'].strftime('%Y%m%d'))
        f = open(filename, 'wb')
        seed = np.random.get_state()
        pickle.dump(seed, f, -1)
        f.close()

        logging.info("Saved the random seed generator values to file")


    def read_random_seed(self, first=False):
        if first:
            filename = self.dasystem['random.seed.init']
            logging.info("Initialised random seed from: %s"%filename)
        else: 
            filename = os.path.join(self['dir.restart'], 'randomseed_%s.pickle' % self['da.restart.tstamp'].strftime('%Y%m%d'))
            logging.info("Retrieved the random seed generator values of last cycle from file")
        f = open(filename, 'rb')
        seed = pickle.load(f,encoding='latin1')
        np.random.set_state(seed)
        f.close()


    def setup(self):
        """ 
        This method determines how to proceed with the cycle. Three options are implemented:

            1. *Fresh start*  : set up the required file structure for this simulation and start
            2. *Restart*      : use latest da_runtime variables from the exec dir and restart
            3. *Recover*      : restart after crash by getting data from restart/one-ago folder

        The choice that gets executed depends on the presence of 

            # the ``-r`` option on the command line, this triggers a recover
            # the ``time.restart : True`` option in the da.rc file

        The latter is automatically set if the filter submits the next cycle at the end of the current one, 
        through method :meth:`~da.tools.initexit.CycleControl.submit_next_cycle`.

        The specific call tree under each scenario is: 

            1. *Fresh Start*
                *  :meth:`~da.tools.initexit.CycleControl.setup_file_structure()`  <- Create directory tree
            2. *Restart*
                *  :meth:`~da.tools.initexit.CycleControl.setup_file_structure()`
                *  :meth:`~da.tools.initexit.CycleControl.random_seed`    <- Read the random seed from file
            3. *Recover*
                *  :meth:`~da.tools.initexit.CycleControl.setup_file_structure()`
                *  :meth:`~da.tools.initexit.CycleControl.recover_run()`          <- Recover files from restart/one-ago dir, reset ``time.start``
                *  :meth:`~da.tools.initexit.CycleControl.random_seed` 

        And is always followed by a call to

            * parse_times()
            * WriteRc('jobfilename')
        """        
        if self['transition']:
            logging.info("Transition of filter from previous step with od meteo from 25 to 34 levels")
            self.setup_file_structure()
            strippedname = os.path.split(self['jobrcfilename'])[-1]
            self['jobrcfilename'] = os.path.join(self['dir.exec'], strippedname)
            self.read_random_seed(False)

        elif self['time.restart']:
            logging.info("Restarting filter from previous step")
            self.setup_file_structure()
            strippedname = os.path.split(self['jobrcfilename'])[-1]
            self['jobrcfilename'] = os.path.join(self['dir.exec'], strippedname)
            self.read_random_seed(False)

        else: #assume that it is a fresh start, change this condition to more specific if crash recover added
            logging.info("First time step in filter sequence")
            self.setup_file_structure()

            # expand jobrcfilename to include exec dir from now on.
            # First strip current leading path from filename

            strippedname = os.path.split(self['jobrcfilename'])[-1]
            self['jobrcfilename'] = os.path.join(self['dir.exec'], strippedname)
            if 'extendedregionsfile' in self.dasystem:
                shutil.copy(os.path.join(self.dasystem['extendedregionsfile']),os.path.join(self['dir.exec'],'da','analysis','cteco2','copied_regions_extended.nc')) 
                logging.info('Copied extended regions file to the analysis directory: %s'%os.path.join(self.dasystem['extendedregionsfile'])) 
            else: 
                shutil.copy(os.path.join(self['dir.exec'],'da','analysis','cteco2','olson_extended.nc'),os.path.join(self['dir.exec'],'da','analysis','cteco2','copied_regions_extended.nc')) 
                logging.info('Copied extended regions within the analysis directory: %s'%os.path.join(self['dir.exec'],'da','analysis','cteco2','olson_extended.nc')) 
            for filename in glob.glob(os.path.join(self['dir.exec'],'da','analysis','cteco2','*.pickle')):
                logging.info('Deleting pickle file %s to make sure the correct regions are used'%os.path.split(filename)[1])
                os.remove(filename) 
            for filename in glob.glob(os.path.join(self['dir.exec'],'*.pickle')):
                logging.info('Deleting pickle file %s to make sure the correct regions are used'%os.path.split(filename)[1])
                os.remove(filename) 
            if 'random.seed.init' in self.dasystem:
                self.read_random_seed(True)

        self.parse_times()
        #self.write_rc(self['jobrcfilename'])

    def setup_file_structure(self):
        """ 
        Create file structure needed for data assimilation system.
        In principle this looks like:

            * ``${{da_rundir}}``
            * ``${{da_rundir}}/input``
            * ``${{da_rundir}}/output``
            * ``${{da_rundir}}/exec``
            * ``${{da_rundir}}/analysis``
            * ``${{da_rundir}}/jobs``
            * ``${{da_rundir}}/restart/current``
            * ``${{da_rundir}}/restart/one-ago``

        .. note:: The exec dir will actually be a simlink to the directory where
                 the observation operator executable lives. This directory is passed through
                 the ``da.rc`` file. 

        .. note:: The observation input files will be placed in the exec dir,
                 and the resulting simulated values will be retrieved from there as well.

        """

# Create the run directory for this DA job, including I/O structure

        filtertime = self['time.start'].strftime('%Y%m%d')

        self['dir.exec'] = os.path.join(self['dir.da_run'], 'exec')
        self['dir.input'] = os.path.join(self['dir.da_run'], 'input')
        self['dir.output'] = os.path.join(self['dir.da_run'], 'output', filtertime)
        self['dir.analysis'] = os.path.join(self['dir.da_run'], 'analysis')
        self['dir.jobs'] = os.path.join(self['dir.da_run'], 'jobs')
        self['dir.restart'] = os.path.join(self['dir.da_run'], 'restart')

        create_dirs(self['dir.da_run'])
        create_dirs(os.path.join(self['dir.exec']))
        create_dirs(os.path.join(self['dir.input']))
        create_dirs(os.path.join(self['dir.output']))
        create_dirs(os.path.join(self['dir.analysis']))
        create_dirs(os.path.join(self['dir.jobs']))
        create_dirs(os.path.join(self['dir.restart']))

        logging.info('Succesfully created the file structure for the assimilation job')


    def finalize(self):
        """
        finalize the da cycle, this means writing the save data and rc-files for the next run. 
        The following sequence of actions occur:

            * Write the randomseed to file for reuse in next cycle
            * Write a new ``rc-file`` with ``time.restart : True``, and new ``time.start`` and ``time.end``
            * Collect all needed data needed for check-pointing (restart from current system state)
            * Move the previous check pointing data out of the way, and replace with current
            * Submit the next cycle

        """
        self.write_random_seed()                              
        self.write_new_rc_file()                              
        
        self.collect_restart_data()  # Collect restart data for next cycle into a clean restart/current folder
        self.collect_output()  # Collect restart data for next cycle into a clean restart/current folder
        self.submit_next_cycle()

    def collect_output(self):
        """ Collect files that are part of the requested output for this cycle. This function allows users to add files 
            to a list, and then the system will copy these to the current cycle's output directory.
            The list of files included is read from the 
            attribute "output_filelist" which is a simple list of files that can be appended by other objects/methods that
            require output data to be saved.


        """
        targetdir = os.path.join(self['dir.output'])
        create_dirs(targetdir)

        logging.info("Collecting the required output data") 
        logging.debug("           to   directory: %s " % targetdir)

        for file in set(self.output_filelist):
            if os.path.isdir(file): # skip dirs
                continue
            if not os.path.exists(file): # skip dirs
                logging.warning("           [not found] .... %s " % file)
                continue

            logging.debug("           [copy] .... %s " % file)
            shutil.copy(file, file.replace(os.path.split(file)[0], targetdir))



    def collect_restart_data(self):
        """ Collect files needed for the restart of this cycle in case of a crash, or for the continuation of the next cycle. 
            All files needed are written to the restart/current directory. The list of files included is read from the 
            attribute "restart_filelist" which is a simple list of files that can be appended by other objects/methods that
            require restart data to be saved.

            .. note:: Before collecting the files in the ``restart_filelist``, the restart/current directory will be emptied and
                     recreated. This prevents files from accumulating in the restart/current and restart/one-ago folders. It 
                     also means that if a file is missing from the ``restart_filelist``, it will not be available for check-pointing
                     if your run crashes or dies!

            Currently, the following files are included:

                * The ``da_runtime.rc`` file
                * The ``randomseed.pickle`` file
                * The savestate.nc file
                * The files in the ``ObservationOperator.restart_filelist``, i.e., restart data for the transport model


            .. note:: We assume that the restart files for the :ref:`ObservationOperator` 
                      reside in a separate folder, i.e, the ObservationOperator does *not* write directly to the CTDAS restart dir!

        """

        targetdir = os.path.join(self['dir.restart'])

        #logging.info("Purging the current restart directory before collecting new data")

        #create_dirs(targetdir, forceclean=True)

        logging.info("Collecting the required restart data")
        logging.debug("           to   directory: %s " % targetdir)

        for file in set(self.restart_filelist):
            if os.path.isdir(file): # skip dirs
                continue
            if not os.path.exists(file): 
                logging.warning("           [not found] .... %s " % file)
            else:
                logging.debug("           [copy] .... %s " % file)
                shutil.copy(file, file.replace(os.path.split(file)[0], targetdir))



#
    def write_new_rc_file(self):
        """ Write the rc-file for the next DA cycle. 

            .. note:: The start time for the next cycle is the end time of this one, while 
                      the end time for the next cycle is the current end time + one cycle length. 
                      
            The resulting rc-file is written to the ``dir.exec`` so that it can be used when resubmitting the next cycle
            
        """
        
        # We make a copy of the current dacycle object, and modify the start + end dates and restart value

        new_dacycle = copy.deepcopy(self)
        new_dacycle['da.restart.tstamp'] = self['time.start']
        new_dacycle.advance_cycle_times()
        new_dacycle['time.restart'] = True
        
        # Create the name of the rc-file that will hold this new input, and write it

        #fname = os.path.join(self['dir.exec'], 'da_runtime.rc')  # current exec dir holds next rc file
        
        fname = os.path.join(self['dir.restart'], 'da_runtime_%s.rc' % new_dacycle['time.start'].strftime('%Y%m%d'))#advanced time
        
        rc.write(fname, new_dacycle)
        logging.debug('Wrote new da_runtime.rc (%s) to restart dir' % fname)

        # The rest is info needed for a system restart, so it modifies the current dacycle object (self)

        self['da.restart.fname'] = fname    # needed for next job template
        #self.restart_filelist.append(fname)  # not that needed since it is already written to the restart dir...
        #logging.debug('Added da_runtime.rc to the restart_filelist for later collection')


    def write_rc(self, fname):
        """ Write RC file after each process to reflect updated info """

        rc.write(fname, self)
        logging.debug('Wrote expanded rc-file (%s)' % fname)
        

    def submit_next_cycle(self):
        """ 
        Submit the next job of a DA cycle, this consists of 
            * Changing to the working directory from which the job was started initially
            * create a line to start the master script again with a newly created rc-file
            * Submitting the jobfile 

        If the end of the cycle series is reached, no new job is submitted.

        """
        

        if self['time.end'] < self['time.finish']:

            # file ID and names
            jobid = self['time.end'].strftime('%Y%m%d') 
            targetdir = os.path.join(self['dir.exec'])
            jobfile = os.path.join(targetdir, 'jb.%s.jb' % jobid)
            logfile = os.path.join(targetdir, 'jb.%s.log' % jobid)
            # Template and commands for job
            jobparams = {{'jobname':"j.%s" % jobid, 'jobnodes':self['da.resources.ntasks'], 'jobtime': self['da.resources.ntime'], 'logfile': logfile, 'errfile': logfile}}
            template = self.daplatform.get_job_template(jobparams)
            execcommand = os.path.join(self['dir.da_submit'], sys.argv[0]) 
            if '-t' in self.opts:
                (self.opts).remove('-t') 

            if 'icycle_in_job' not in os.environ:
                logging.info('Environment variable icycle_in_job not found, resubmitting after this cycle')
                os.environ['icycle_in_job'] = self['da.resources.ncycles_per_job']  # assume that if no cycle number is set, we should submit the next job by default
            else:
                logging.info('Environment variable icycle_in_job was found, processing cycle %s of %s in this job'%(os.environ['icycle_in_job'],self['da.resources.ncycles_per_job']) )

            ncycles = int(self['da.resources.ncycles_per_job'])
            for cycle in range(ncycles): 
                nextjobid = '%s'% ( (self['time.end']+cycle*self['cyclelength']).strftime('%Y%m%d'),)
                nextrestartfilename = self['da.restart.fname'].replace(jobid,nextjobid)
                nextlogfilename = logfile.replace(jobid,nextjobid)
                if self.daplatform.ID == 'WU capegrim':
                    template += """\nexport icycle_in_job=%d\npython3 %s rc=%s %s >&%s &\n""" % (cycle+1,execcommand, nextrestartfilename, ''.join(self.opts), nextlogfilename,)
                else: 
                    template += """\nexport icycle_in_job=%d\npython3 %s rc=%s %s >&%s\n""" % (cycle+1,execcommand, nextrestartfilename, ''.join(self.opts), nextlogfilename,) 

            # write and submit 
            self.daplatform.write_job(jobfile, template, jobid)
            if 'da.resources.ncycles_per_job' in self:
                do_submit = (int(os.environ['icycle_in_job']) >= int(self['da.resources.ncycles_per_job']))
            else:
                dosubmit = False
          
            if do_submit:
                jobid = self.daplatform.submit_job(jobfile, joblog=logfile)

        else:
            logging.info('Final date reached, no new cycle started')


def start_logger(level=logging.INFO):
    """ start the logging of messages to screen"""

# start the logging basic configuration by setting up a log file

    logging.basicConfig(level=level,
                        format=' [%(levelname)-7s] (%(asctime)s) py-%(module)-20s : %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')

def parse_options():
    """ 
    Function parses options from the command line and returns the arguments as a dictionary.
    Accepted command line arguments are:

    ========  =======
    Argument  Meaning
    ========  =======
    -v        verbose output in log files
    -h        display help
    -r        start a simulation by recovering from a previous crash
    -t        start a simulation by transitioning from 25 to 34 layers in December 2005 (od meteo)
    ========  =======

    """

# Parse keywords, the only option accepted so far is the "-h" flag for help

    opts = []
    args = []
    try:                                
        opts, args = getopt.gnu_getopt(sys.argv[1:], "-rvt")
    except getopt.GetoptError as msg:           
        logging.error('%s' % msg)
        sys.exit(2)      

    for options in opts:
        options = options[0].lower()
        if options == '-r':
            logging.info('-r flag specified on command line: recovering from crash')
        if options == '-t':
            logging.info('-t flag specified on command line: transition with od from December 2005')    
        if options == '-v':
            logging.info('-v flag specified on command line: extra verbose output')
            logging.root.setLevel(logging.DEBUG)

    if opts: 
        optslist = [item[0] for item in opts]
    else:
        optslist = []

# Parse arguments and return as dictionary

    arguments = {{}}
    for item in args:
        #item=item.lower()

# Catch arguments that are passed not in "key=value" format

        if '=' in item:
            key, arg = item.split('=')
        else:
            logging.error('%s' % 'Argument passed without description (%s)' % item)
            raise getopt.GetoptError(arg)

        arguments[key] = arg


    return optslist, arguments

def validate_opts_args(opts, args):
    """ 
 Validate the options and arguments passed from the command line before starting the cycle. The validation consists of checking for the presence of an argument "rc", and the existence of
 the specified rc-file.  
 
    """
    if "rc" not in args:
        msg = "There is no rc-file specified on the command line. Please use rc=yourfile.rc"
        logging.error(msg)
        raise IOError(msg)
    elif not os.path.exists(args['rc']):
        msg = "The specified rc-file (%s) does not exist " % args['rc'] 
        logging.error(msg)
        raise IOError(msg)

    # WP not needed anymore
    #if not args.has_key('process'):
    #    msg = "There is no process specified on the command line, assuming process=Start"   ; logging.info(msg)
    #    args['process'] = 'start'
    #if args['process'].lower() not in validprocesses:
    #    msg = "The specified process (%s) is not valid"%args['process']   ; logging.error(msg)
    #    raise IOError,msg

    return opts, args


if __name__ == "__main__":
    pass

