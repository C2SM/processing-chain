import os
import types


user = os.environ['USER']
mail_address = {
    'gkuhl':    'gerrit.kuhlmann@empa.ch',
    'kug':      'gerrit.kuhlmann@empa.ch',
    'dbrunner': 'dominik.brunner@empa.ch'
}[user]


# Everything defined before is not written to os.environ
# and thus not available to bash scripts.
not_config = list(locals().keys())


compute_host = 'daint'
compute_queue = 'normal' #'debug' #'normal'
compute_account = 'd73' #'pr04'

# input root
input_root = '/project/kug/cosmo/test/input'
meteo_dir = os.path.join(input_root, 'meteo')

# output
output_root = '/project/kug/cosmo/test/output'

# working root
work_root = '/project/kug/cosmo/test/run/'
log_dir = os.path.join(work_root, 'logs')


# anthropogenic emissions pre-processed for mother and nested domain
emis_dir_mother = os.path.join(input_root, 'emissions')
emis_dir_nest = os.path.join(input_root, 'emissions')
emis_gridname = "Berlin2"

# VPRM biogenic fluxes
#vprm_dir_orig = os.path.join(input_root, 'vprm')
#vprm_dir_proc = os.path.join(input_root, 'vprm', 'processed')
vprm_dir_orig = '/project/d73/input/vprm_smartcarb/'
vprm_dir_proc = '/project/d73/input/vprm_smartcarb/processed/'

# CAMS for CO2, CO and NOX initial and boundary conditions
cams_dir_orig = os.path.join(input_root, 'icbc')
#cams_dir_proc = os.path.join(input_root, 'icbc', 'processed') 
cams_dir_proc = os.path.join(input_root, 'icbc', 'processed2')


# chain root (TODO: remove)
chain_src_dir = '/project/d73/%s/processing_chain/' % user
chain_src_dir = '/users/%s/projects/cosmo/processing_chain/' % user
tools_dir = os.path.join(chain_src_dir, 'tools')

# some constants for scripts
del_tmp_timeout = 120   # timeout in days after which temporary files 
                        # like restarts (last forecast hour of day - 1) are removed
                        # by "purgetmp"

meteo_spinup = 0        # time in hours the model is integrated before transport simulation 
                        # is started (i.e. spinupstart = inidate - meteospinup)
                        # NOTE: has to be multiple of meteo increment (e.g 3hrs for IFS)


# INT2LM
int2lm_extpar_dir = os.path.join(input_root, 'extpar')
int2lm_extpar_file = 'domain_berlin_big.nc'
int2lm_bin = '/users/%s/projects/cosmo/int2lm/int2lm' % user

# COSMO
cosmo_bin= '/users/%s/projects/cosmo/cosmo-pompa/cosmo/cosmo' % user

# Case specific settings (int2lm and cosmo namelists and runscripts)
int2lm_namelist = '%s/cases/berlin2_int2lm_INPUT.sh' % chain_src_dir
int2lm_runjob = '%s/cases/berlin2_int2lm_runjob.sh' % chain_src_dir
cosmo_namelist = '%s/cases/berlin2_cosmo_INPUT.sh' % chain_src_dir
cosmo_runjob = '%s/cases/berlin2_cosmo_runjob.sh' % chain_src_dir


# add local variables (defined after "not_config") to os.environ to be used
# in bash scripts called by this script
for key, value in list(locals().items()):
    if key != 'not_config' and key not in not_config:
        os.environ[key] = str(value)










