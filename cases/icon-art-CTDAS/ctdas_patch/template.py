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

#################################################################################################
# First order of business is always to make all other python modules accessible through the path
#################################################################################################

import sys
import os
import logging
sys.path.append(os.getcwd())

#################################################################################################
# Next, import the tools needed to initialize a data assimilation cycle
#################################################################################################

from da.cyclecontrol.initexit_cteco2 import start_logger, validate_opts_args, parse_options, CycleControl
from da.pipelines.pipeline_icon import ensemble_smoother_pipeline, header, footer, analysis_pipeline, archive_pipeline
from da.dasystems.dasystem_baseclass import DaSystem
from da.platform.pizdaint import PizDaintPlatform
from da.statevectors.statevector_baseclass_icos_cities import StateVector 
from da.observations.obs_class_ICOS_OCO2 import ICOSObservations, TotalColumnObservations # Here we set which observations we consider!
from da.obsoperators.obsoperator_ICOS_OCO2 import ObservationOperator  # Here we set the obs-operator, which should sample the same observations!
from da.optimizers.optimizer_baseclass_icos_cities import Optimizer


#################################################################################################
# Parse and validate the command line options, start logging
#################################################################################################

start_logger()
opts, args = parse_options()
opts, args = validate_opts_args(opts, args)

#################################################################################################
# Create the Cycle Control object for this job    
#################################################################################################

dacycle = CycleControl(opts, args)

platform    = PizDaintPlatform()
dasystem    = DaSystem(dacycle['da.system.rc'])
obsoperator = ObservationOperator(dacycle['da.system.rc'])
samples     = [ICOSObservations(), TotalColumnObservations()]
statevector = StateVector()
optimizer   = Optimizer()

##########################################################################################
################### ENTER THE PIPELINE WITH THE OBJECTS PASSED BY THE USER ###############
##########################################################################################


logging.info(header + "Entering Pipeline " + footer) 

ensemble_smoother_pipeline(dacycle, platform, dasystem, samples, statevector, obsoperator,optimizer)


##########################################################################################
################### All done, extra stuff can be added next, such as analysis
##########################################################################################

sys.exit(0)

logging.info(header + "Starting analysis" + footer) 

analysis_pipeline(dacycle, platform, dasystem, samples, statevector )

sys.exit(0)


