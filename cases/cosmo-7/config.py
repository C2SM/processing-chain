import os
import types
import sys

year = sys.argv[2][0:4]

user = os.environ['USER']

# Everything defined before is not written to os.environ
# and thus not available to bash scripts.
not_config = list(locals().keys())

print(sys.argv)

target = 'cosmo'
restart_step = 24
subtarget = 'spinup'
spinup = 0

compute_host = 'daint'
compute_queue = 'normal'  #'debug' #'normal'

compute_account = 'em05'  #'sd02' #'sd02'
constraint = 'mc'

if constraint == 'gpu':
    ntasks_per_node = 12
    mpich_cuda = ('export MPICH_RDMA_ENABLED_CUDA=1\n'
                  'export MPICH_G2G_PIPELINE=256\n'
                  'export CRAY_CUDA_MPS=1\n')
elif constraint == 'mc':
    ntasks_per_node = 36
    mpich_cuda = ''

# case name = pathname in cases/
path = os.path.realpath(__file__)
casename = os.path.basename(os.path.dirname(path))

# meteo
#meteo_dir = '/store/mch/msopr/owm/IFS-HRES-BC/IFS-HRES-BC20' # before 2020-10-28
#meteo_dir = '/store/empa/em05/COSMO7_BC/' # until 2020-11-11 12
#meteo_dir = '/store/s83/osm/IFS-HRES-BC-EMPA/IFS-HRES-BC-EMPA20' # after 2020-11-11 12
meteo_dir = '/store/s83/osm/IFS-HRES-BC-EMPA/IFS-HRES-BC-EMPA21'
meteo_dir_alt = '/store/mch/msopr/owm/IFS-HRES-BC/IFS-HRES-BC20'
meteo_prefix = "efsf"
meteo_inc = 1

# output
output_root = '/store/empa/em05/cosmo-7/output'

# working root
work_root = os.environ['SCRATCH'] + "/processing_chain"
log_dir = os.path.join(work_root, 'logs')

# chain root
chain_src_dir = os.getcwd()
tools_dir = os.path.join(chain_src_dir, 'jobs/tools')

# INT2LM
int2lm_extpar_dir = '/store/empa/em05/cosmo-7/extpar'
int2lm_extpar_file = 'lmExtPara_601x601_0.06_20090226'
int2lm_bin = '/store/empa/em05/executables/int2lm_gnu_208d68e_20201005'

# COSMO
cosmo_bin = '/store/empa/em05/executables/cosmo-pompa_cosmo7_container_gnu_4d8c5473_20210207'

# FIELDEXTRA
laf_startfile = '/store/mch/msopr/owm/COSMO-7/ANA20/laf2020102212'
fieldextra_bin = '/store/empa/em05/executables/fieldextra_gnu_opt_omp_2dced5a5_20210107'
fieldextra_control_file = '%s/cases/%s/merge.ctl' % (chain_src_dir, casename)
do_merge_at_start = True

# Case specific settings (int2lm and cosmo namelists and runscripts)
int2lm_namelist = '%s/cases/%s/int2lm_INPUT.cfg' % (chain_src_dir, casename)
int2lm_runjob = '%s/cases/%s/int2lm_runjob.cfg' % (chain_src_dir, casename)
cosmo_namelist = '%s/cases/%s/cosmo_INPUT_' % (chain_src_dir, casename)
cosmo_runjob = '%s/cases/%s/cosmo_runjob.cfg' % (chain_src_dir, casename)

# Observation Nudging
obs_nudging_dir = '/store/empa/em05/obs_nudging_cosmo'
obs_nudging_prefixes = [
    'cdfin_amdar', 'cdfin_buoy', 'cdfin_ship', 'cdfin_synop', 'cdfin_temp',
    'cdfin_wprof'
]
#obs_nudging_date_format = "-%Y%m%d%H%M%S"
obs_nudging_date_format = "-%Y%m%d000000"

# Walltimes and domain decomposition

## INT2LM
if compute_queue == "normal":
    int2lm_walltime = "00:30:00"
elif compute_queue == "debug":
    int2lm_walltime = "00:30:00"
else:
    logging.error("Unset queue name: %s" % compute_queue)
    sys.exit(1)

int2lm_nodes = 2
int2lm_np_x = 6
int2lm_np_y = 4
int2lm_np_tot = int2lm_np_x * int2lm_np_y

## COSMO
if compute_queue == "normal":
    cosmo_walltime = "16:00:00"
    cosmo_np_x = 9
    cosmo_np_y = 8
elif compute_queue == "debug":
    cosmo_walltime = "00:30:00"
    cosmo_np_x = 2
    cosmo_np_y = 2
else:
    logging.error("Unknown queue name: %s" % compute_queue)
    sys.exit(1)

# Total node count
cosmo_np_io = 0
cosmo_np_tot = int(cosmo_np_x * cosmo_np_y / ntasks_per_node) + cosmo_np_io
