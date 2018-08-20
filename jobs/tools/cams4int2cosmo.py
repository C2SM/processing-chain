#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Prepare CAMS CO2, CO and NOx boundary conditions for int2lm/int2cosmo
# for the project SMARTCARB.
# The CAMS data consists of 
#   - CO2 and CO fields of experiment gf39, class RD (approx. 15 km resolution, 137 levels)
#     https://atmosphere.copernicus.eu/change-log-gf39
#   - NO and NO2 from the CAMS operational product, exp 0001, class MC (60 km resolution)
#     
#
# The data sets are retrieved as individual 3-hourly files from the MARS archive at ECMWF
# with names
#  cams_0001_2015010500.nc   for NO and NO2
#  sfc_0001_2015010500.nc    for log of surface pressure
#  cams_gf39_2015010500.nc   for CO and CO2
#  sfc_gf39_2015010500.nc    for log of surface pressure
#
# The path to the directory of the CAMS data can optionally be supplied, 
# otherwise the script should be invoked in the directory of the CAMS data.
#
# The script generates 8 individual 3-hourly IC/BC files 
#   cams_nox_yyyymmddhh.nc
#   cams_co2_yyyymmddhh.nc
#
# Usage: cams4int2cosmo.sh date [-i inpath -o outpath]
#      date : in format YYYYMMDD
#      -i inpath  : path of original CAMS files (default is current path)
#      -o outpath : path where output files should be generated (default is current path)
#
# Output: 
#      cams_NOX_YYYYMMDD00.nc to cams_NOX_YYYYMMDD21.nc
#      cams_CO2_YYYYMMDD00.nc to cams_CO2_YYYYMMDD21.nc
#
# Author: Dominik Brunner (DB), Empa, Switzerland
#       DB, 15 May 2017: first implementation on daint.cscs.ch
#       DB, 06 Jun 2017: modified to add surface pressure fields
#       hjm, 22 Jun 2018: Translated to Python


######################
## Problems:
## - when comparing to the "processed2" icbc form Gerrit, PSURF is slightly different
## - when checking the website, the b levels are not the same as in CAMS
## - It is not possible to delete a variable from a netcdf4 file, so resort to calling ncks at the end

import argparse
import os
import logging
import shutil
import netCDF4 as nc
import numpy as np
from subprocess import call
import sys
import importlib

###############################
##  Constants : CAMS levels  ##
###############################
# hybrid coefficients of the 137 model level version
# http://www.ecmwf.int/en/forecasts/documentation-and-support/137-model-levels
a_half_137=[0.0000000000e+00, 2.0003650188e+00, 3.1022410393e+00, 4.6660838127e+00, 6.8279771805e+00,
           9.7469663620e+00, 1.3605423927e+01, 1.8608930588e+01, 2.4985717773e+01, 3.2985710144e+01,
           4.2879241943e+01, 5.4955463409e+01, 6.9520576477e+01, 8.6895881653e+01, 1.0741574097e+02,
           1.3142550659e+02, 1.5927940369e+02, 1.9133856201e+02, 2.2796894836e+02, 2.6953958130e+02,
           3.1642074585e+02, 3.6898236084e+02, 4.2759249878e+02, 4.9261602783e+02, 5.6441345215e+02,
           6.4333990479e+02, 7.2974414062e+02, 8.2396783447e+02, 9.2634490967e+02, 1.0372011719e+03,
           1.1568536377e+03, 1.2856103516e+03, 1.4237701416e+03, 1.5716229248e+03, 1.7294489746e+03,
           1.8975192871e+03, 2.0760959473e+03, 2.2654316406e+03, 2.4657705078e+03, 2.6773481445e+03,
           2.9003913574e+03, 3.1351193848e+03, 3.3817436523e+03, 3.6404682617e+03, 3.9114904785e+03,
           4.1949306641e+03, 4.4908173828e+03, 4.7991494141e+03, 5.1198950195e+03, 5.4529907227e+03,
           5.7983447266e+03, 6.1560742188e+03, 6.5269467773e+03, 6.9118706055e+03, 7.3118691406e+03,
           7.7274121094e+03, 8.1593540039e+03, 8.6085253906e+03, 9.0764003906e+03, 9.5626826172e+03,
           1.0065978516e+04, 1.0584631836e+04, 1.1116662109e+04, 1.1660067383e+04, 1.2211547852e+04,
           1.2766873047e+04, 1.3324668945e+04, 1.3881331055e+04, 1.4432139648e+04, 1.4975615234e+04,
           1.5508256836e+04, 1.6026115234e+04, 1.6527322266e+04, 1.7008789062e+04, 1.7467613281e+04,
           1.7901621094e+04, 1.8308433594e+04, 1.8685718750e+04, 1.9031289062e+04, 1.9343511719e+04,
           1.9620042969e+04, 1.9859390625e+04, 2.0059931641e+04, 2.0219664062e+04, 2.0337863281e+04,
           2.0412308594e+04, 2.0442078125e+04, 2.0425718750e+04, 2.0361816406e+04, 2.0249511719e+04,
           2.0087085938e+04, 1.9874025391e+04, 1.9608572266e+04, 1.9290226562e+04, 1.8917460938e+04,
           1.8489707031e+04, 1.8006925781e+04, 1.7471839844e+04, 1.6888687500e+04, 1.6262046875e+04,
           1.5596695312e+04, 1.4898453125e+04, 1.4173324219e+04, 1.3427769531e+04, 1.2668257812e+04,
           1.1901339844e+04, 1.1133304688e+04, 1.0370175781e+04, 9.6175156250e+03, 8.8804531250e+03,
           8.1633750000e+03, 7.4703437500e+03, 6.8044218750e+03, 6.1685312500e+03, 5.5643828125e+03,
           4.9937968750e+03, 4.4573750000e+03, 3.9559609375e+03, 3.4892343750e+03, 3.0572656250e+03,
           2.6591406250e+03, 2.2942421875e+03, 1.9615000000e+03, 1.6594765625e+03, 1.3875468750e+03,
           1.1432500000e+03, 9.2650781250e+02, 7.3499218750e+02, 5.6806250000e+02, 4.2441406250e+02,
           3.0247656250e+02, 2.0248437500e+02, 1.2210156250e+02, 6.2781250000e+01, 2.2835937500e+01,
           3.7578129768e+00, 0.0000000000e+00, 0.0000000000e+00]
b_half_137=[0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00,
           0.0000000000e+00, 0.0000000000e+00, 3.8199999608e-08, 6.7607002165e-06, 2.4348000807e-05,
           5.8921999880e-05, 1.1191429803e-04, 1.9857739971e-04, 3.4037968726e-04, 5.6155532366e-04,
           8.8969792705e-04, 1.3528055279e-03, 1.9918379840e-03, 2.8571242001e-03, 3.9709536359e-03,
           5.3778146394e-03, 7.1333767846e-03, 9.2614600435e-03, 1.1806022376e-02, 1.4815628529e-02,
           1.8318451941e-02, 2.2354844958e-02, 2.6963520795e-02, 3.2176095992e-02, 3.8026399910e-02,
           4.4547960162e-02, 5.1773015410e-02, 5.9728413820e-02, 6.8448252976e-02, 7.7958308160e-02,
           8.8285736740e-02, 9.9461667240e-02, 1.1150465161e-01, 1.2444812804e-01, 1.3831289113e-01,
           1.5312503278e-01, 1.6891041398e-01, 1.8568944931e-01, 2.0349121094e-01, 2.2233286500e-01,
           2.4224400520e-01, 2.6324188709e-01, 2.8535401821e-01, 3.0859845877e-01, 3.3293908834e-01,
           3.5825419426e-01, 3.8436332345e-01, 4.1112476587e-01, 4.3839120865e-01, 4.6600329876e-01,
           4.9380031228e-01, 5.2161920071e-01, 5.4930114746e-01, 5.7669216394e-01, 6.0364806652e-01,
           6.3003581762e-01, 6.5573596954e-01, 6.8064302206e-01, 7.0466899872e-01, 7.2773873806e-01,
           7.4979656935e-01, 7.7079755068e-01, 7.9071676731e-01, 8.0953603983e-01, 8.2725608349e-01,
           8.4388113022e-01, 8.5943180323e-01, 8.7392926216e-01, 8.8740754128e-01, 8.9990049601e-01,
           9.1144818068e-01, 9.2209565639e-01, 9.3188077211e-01, 9.4085955620e-01, 9.4906443357e-01,
           9.5654952526e-01, 9.6335172653e-01, 9.6951341629e-01, 9.7507840395e-01, 9.8007160425e-01,
           9.8454189301e-01, 9.8849952221e-01, 9.9198400974e-01, 9.9500250816e-01, 9.9763011932e-01,
           1.0000000000e+00]

# hybrid coefficients of the 60 model level version
# http://www.ecmwf.int/en/forecasts/documentation-and-support/60-model-levels
a_half_60=[0.0000000000e+00, 2.0000000000e+01, 3.8425338745e+01, 6.3647796631e+01, 9.5636962891e+01, 
  1.3448330688e+02, 1.8058435059e+02, 2.3477905273e+02, 2.9849584961e+02, 3.7397192383e+02, 
  4.6461816406e+02, 5.7565112305e+02, 7.1321801758e+02, 8.8366040039e+02, 1.0948347168e+03, 
  1.3564746094e+03, 1.6806403809e+03, 2.0822739258e+03, 2.5798886719e+03, 3.1964216309e+03, 
  3.9602915039e+03, 4.9067070312e+03, 6.0180195312e+03, 7.3066328125e+03, 8.7650546875e+03, 
  1.0376125000e+04, 1.2077445312e+04, 1.3775324219e+04, 1.5379804688e+04, 1.6819472656e+04, 
  1.8045183594e+04, 1.9027695312e+04, 1.9755109375e+04, 2.0222203125e+04, 2.0429863281e+04, 
  2.0384480469e+04, 2.0097402344e+04, 1.9584328125e+04, 1.8864750000e+04, 1.7961359375e+04, 
  1.6899468750e+04, 1.5706449219e+04, 1.4411125000e+04, 1.3043218750e+04, 1.1632757812e+04, 
  1.0209500000e+04, 8.8023554688e+03, 7.4388046875e+03, 6.1443164062e+03, 4.9417773438e+03, 
  3.8509133301e+03, 2.8876965332e+03, 2.0637797852e+03, 1.3859125977e+03, 8.5536181641e+02, 
  4.6733349609e+02, 2.1039389038e+02, 6.5889236450e+01, 7.3677425385e+00, 0.0000000000e+00, 
  0.0000000000e+00]
b_half_60=[0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 
  0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 
  0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 
  0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 
  0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 0.0000000000e+00, 
  7.5823496445e-05, 4.6139489859e-04, 1.8151560798e-03, 5.0811171532e-03, 1.1142909527e-02, 
  2.0677875727e-02, 3.4121163189e-02, 5.1690407097e-02, 7.3533833027e-02, 9.9674701691e-02, 
  1.3002252579e-01, 1.6438430548e-01, 2.0247590542e-01, 2.4393314123e-01, 2.8832298517e-01, 
  3.3515489101e-01, 3.8389211893e-01, 4.3396294117e-01, 4.8477154970e-01, 5.3570991755e-01, 
  5.8616840839e-01, 6.3554745913e-01, 6.8326860666e-01, 7.2878581285e-01, 7.7159661055e-01, 
  8.1125342846e-01, 8.4737491608e-01, 8.7965691090e-01, 9.0788388252e-01, 9.3194031715e-01, 
  9.5182150602e-01, 9.6764522791e-01, 9.7966271639e-01, 9.8827010393e-01, 9.9401944876e-01, 
  9.9763011932e-01, 1.0000000000e+00]



def main(date,inpath,outpath,param):
    try:
        os.makedirs(outpath, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating output directory failed")
        raise

    tracer2dict = {
            'CO2': dict(name = "CO2_BG", short_name = "co2", long_name= "carbon_dioxide"),
            'CO':  dict(name = "CO_BG", short_name = "co", long_name= "carbon_monoxide"),
            'CH4': dict(name = "CH4_BG", short_name = "ch4", long_name= "methane"),
            'NOX': dict(name = "NOX_BG", short_name = "NOx", long_name= "nitrogen_oxide"),
    }

    species = []
    for s in param["species"]:
        try:
            s.append(tracer2dict[s])
        except KeyError:
            logging.error("Variable "+s+" is not part of the available list of variables.")

    main_process(date,inpath,outpath,species,param)


def main_process(date,inpath,outpath,species,param):    
    if param["lev"]==137:
        a_half = a_half_137 #defined globally
        b_half = b_half_137 #defined globally
        offset = 37 # only levels 38 to 137 are retrieved from MARS
    elif param["lev"]==60:
        a_half = a_half_60 #defined globally
        b_half = b_half_60 #defined globally
        offset = 0 # all levels
    else:
        logging.error("The list of hybrid parameters for the amount of levels " + param["lev"] + "is not defined.")
        raise

    hyam=[(a_half[i]+a_half[i+1])/2. for i in range(offset,len(a_half)-1)]
    hybm=[(b_half[i]+b_half[i+1])/2. for i in range(offset,len(a_half)-1)]   
    
    to_print = ",".join([species[i]["short_name"] for i in range(len(species))])
    logging.info('Processing',to_print,'for time',date)
    
    infile=os.path.join(inpath,param["prefix1"]+"_"+date.strftime("%Y%m%d%H")+".nc")
    sfcfile=os.path.join(inpath,param["prefix2"]+"_"+date.strftime("%Y%m%d%H")+".nc")    
    
    outfile = os.path.join(outpath,param["suffix"]+"_"+date.strftime("%Y%m%d%H")+".nc")

    try:
        shutil.copy(infile, outfile)
    except FileNotFoundError:
        logging.error("file "+infile+" not found")
        raise
    except (PermissionError, OSError):
        logging.error("Copying file "+infile +" failed")
        raise        
    
    with nc.Dataset(outfile,"a",format="NETCDF4") as outf:
        # copy surface pressure from surface file
        with nc.Dataset(sfcfile,"r") as inf:
            lnsp = inf.variables["lnsp"]
            outf.createVariable("PSURF","f8",lnsp.dimensions) 
            outf.variables["PSURF"][:] = np.exp(lnsp[:])
            outf["PSURF"].setncattr("long_name","surface pressure")
            outf["PSURF"].setncattr("units","Pa")

        # change the calender attribute to proleptic_gregorian
        outf["time"].setncattr("calendar","proleptic_gregorian")
                
        # rename fields according to Carbosense4D definitions
        for s in species:
            if s["name"]!="NOX_BG":
                outf.renameVariable(s["short_name"],s["name"])
            else:
                # Create NOX from NO and NO2
                outf.createVariable("NOX_BG","f8",outf["no"].dimensions)
                outf["NOX_BG"][:] = outf["no"][:]+outf["no2"][:]

        outf.renameDimension("latitude","lat")
        outf.renameDimension("longitude","lon")
        outf.renameVariable("latitude","lat")
        outf.renameVariable("longitude","lon")

        outf["lat"][:] = outf["lat"][::-1]
        
        # Add the units; these must match those in $int2cosmo/src/trcr_gribtabs.f90
        for s in species:
            chem = s["name"]
            long = s["long_name"]
            outf[chem].setncattr("units","kg kg-1")
            outf[chem].setncattr("units_desc","kg of substance per kg of dry air")
            outf[chem].setncattr("long_name","mass mixing ratio of "+chem[:-3]+" from outside domain")
            outf[chem].setncattr("standard_name","mass_fraction_of_"+long+"_in_air")

        # Add hybrid coefficients
        outf.createVariable("hyam",np.float64,"level")
        outf.createVariable("hybm",np.float64,"level")
        outf["hyam"][:]=hyam
        outf["hybm"][:]=hybm
        outf["hyam"].setncattr("units","Pa")
        outf["hybm"].setncattr("units","1")
        outf["hyam"].setncattr("long_name","hybrid A coefficient at layer midpoints")
        outf["hybm"].setncattr("long_name","hybrid B coefficient at layer midpoints")

            
        # create new dimension lev1
        outf.createDimension("lev1",1)
        outf.createVariable("P0",float,"lev1")
        outf["P0"][:]=[1]
        outf["P0"].setncattr("units","Pa")
        outf["P0"].setncattr("long_name","reference pressure")

        #  if NOX_BG, delete no and no2 
        for s in species:
            if s["name"]=="NOX_BG":
                if "no" in outf.variables and "no2" in outf.variables:                    
                    call(["ncks", "--overwrite", "-x", "-v", "no,no2", outfile, outfile])
                
    
if __name__ == "__main__":    
    parser = argparse.ArgumentParser(description="Prepare CAMS CO2, CO, CH4 and NOx boundary conditions for int2lm/int2cosmo",
         formatter_class=argparse.RawTextHelpFormatter
         )
 
    parser.add_argument('date',  type=str, help='date in format YYYYMMDD')
    parser.add_argument('param',  type=str, help='dictionary of the parameters')
    parser.add_argument('-i', type=str, metavar="inpath",
                     help='path of original CAMS files (default is current path)',
                     default=os.getcwd())
    parser.add_argument('-o', type=str,metavar="outpath",
                        help="path where output files should be generated (default is inpath/processed)",
                     )

    indir = parser.get_default("i")
    parser.set_defaults(o=os.path.join(indir,"processed"))

    args = parser.parse_args()    

    main(args.date,args.i,args.o,args.param)
    
