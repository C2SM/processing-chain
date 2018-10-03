#!/bin/bash

#
# MOZART-GEOS5 -> INT2LM pre-preprocessor
#
# Conversion script to use atmospheric chemistry model data from
# http://www.acd.ucar.edu/wrf-chem/mozart.shtml in the INT2LM preprocessor.
#
# UCAR should deliver a set of 4D variables on a regular lat-lon grid packed
# into one NetCDF file. (e.g. "moztrcr_data.nc")
#
# Software dependencies:
#
# - to be run from a bash shell
# - cdo (Climate data operators), version > 1.3, with NetCDF support builtin
# - ncdump, ncgen (usually installed when NetCDF library is compiled)
# - nco (http://nco.sourceforge.net)
# - standard UNIX utilities: grep, find, ...
#
# Calling example:  mozartNcep2int2lm moztrcr_data.nc
#
# Delivers: Files called "mozart_YYYYMMDDHH.nc" which can be fed directly to
#           INT2LM
#
# Example INT2LM namelists (1 for gasphase, 1 for aerosols):
#
#&TRCRDATASET
#  startlon_tot=-180, dlon=2.81250,  
#  startlat_tot=-90, dlat=2.85715,   
#  ie_tot=128, je_tot=64, ke_tot=28, 
#  yvertical_axis_type="hyb_sig_pr", yvertical_method="I",
#  ylfn_prefix = "mozart_",
#  yvarlist= "N2O5", "NH3", "NO2", "NO3", "NO", "OZONE", "OH", "ONIT", 
#  "PAN", "TOL","PAA", "OP1", "CO", "CSL", "GLY", "H2O2", "HNO3", "HO2", "ISO", 
#  "MPAN", "MACR", "KET", "ALD", "HC5", "OL2", "OP2", "ETH", 
#  "HC3", "HCHO", "MGLY", "ORA2", ycombine_action = "O",
#  hinc=6.0, lconst_in_time=.FALSE., lconserve_mass=.FALSE., yinterp_type="N",
#  ydirin="<input directory>",
#/
#
# Author:
#
# Christoph Knote, Empa, Switzerland, 04.2010
# christoph.knote@empa.ch
#
# Version:
# 1.0 CK Initial release
# 1.5 CK Added calculation of pressure field, aerosol variables
# 1.6 CK Corrected treatment of dates in input file (gregorian -> proleptic_gregorian)
# 2.0 CK Major cleanup, removed R dependency, removed (unused) aerosol treatment
# 2.1 CK removed GLYALD, CHCOCH3, MVK for MACC2 processing
# 2.2 CK really adapted it to be useable with IFS-MOZART data
# 12.06.2018 muq adapted it for MOZART-GEOS5 data
# 03.10.2018 dao adapted it to take in-and output as parameters
#
# License:
# Feel free to use, please send useful modifications to the author.
#
# Note from the author:
# This is a quick script with a lot of dependencies, assumptions and akwardness - handle with care...
#
# Words of warning:
# * We have now used this script in several simulations and assessments and
#   it seems to do what it should. Be careful however to make sure that
#   all software needed (stated above) is installed and working properly.
#   There is not much error handling implemented, so it might silently fail
#   if necessary software is missing...
#

#mozart2int2lm() {

###############################################################################
# 1) Preliminaries
###############################################################################
   ncap2 -s 'where(lon>180) lon=lon-360' mozart4geos5_20150625-20150720_Europe.nc mozart4geos5_20150203-20150221_Europe_LonAdjust.nc 

   mozartFile=$1
   outputPath=$2
   
  # ################################################
  # load modules at hypatia
  # module load netcdf/4.3.0
  # module load cdo/1.8.0rc5
  # module load nco/4.6.3/gcc 
  #
  # load modules at daint
  # module load cray-netcdf
  # module load NCO
  # module load CDO 
  ##################################################

   if [[ ! -e $mozartFile ]]
   then
     echo "Input file $mozartFile missing. Aborting."
     return
   fi

   workDir=$(pwd)
   mozartNcepTmpDir=${workDir}/mozartPreproc_$(date +%s)/

   # check if file has already been pre-processed
   ncdump -h $mozartFile | grep -q "time:calendar = \"proleptic_gregorian\""   
   calendarOK=$?
   if [[ $calendarOK -ne 0 ]]
   then
     # need to update calendar attribute so cdo calculates the correct dates
     ncatted -O -a calendar,time,o,c,"proleptic_gregorian" ${mozartFile}
   fi

   mkdir -p $mozartNcepTmpDir

###############################################################################
# 6) Split into single timesteps
###############################################################################

   # this is done iteratively, as CDOs cannot do this by default
   cdo -s splityear ${mozartFile} ${mozartNcepTmpDir}year

   cd ${mozartNcepTmpDir}
   for aYear in $(ls -1 year*)
   do
     cdo -s splitmon $aYear ${aYear%.nc}month
   done
   for aMonth in $(ls -1 year*month*)
   do
     cdo -s splitday $aMonth ${aMonth%.nc}day
   done
   for aDay in $(ls -1 year*month*day*)
   do
       cdo -s splithour $aDay ${aDay%.nc}hour
   done

   find . -not -name "*hour*.nc" -delete

###############################################################################
# 7) date change function
###############################################################################

   # this script updates the date in the file to match the real date
   chdate() {
     local myfile=$1

     ncap -O -s time=0.0 ${myfile} ${myfile}

     local datestring=$(ncdump -v date $myfile | grep -o "date = [0-9]\{8\}")
     local dateyear=${datestring:7:4}
     local datemonth=${datestring:11:2}
     local dateday=${datestring:13:2}

     local secsince=$(ncdump -v datesec $myfile | grep -o "datesec = [0-9 ]\{6\}")
     local secsince=${secsince:10:6}
     local hrssince=0
     let hrssince=secsince/3600
     local hrssince_nice=$(printf "%02d" $hrssince)

     local units="seconds since "${dateyear}"-"${datemonth}"-"${dateday}" "${hrssince_nice}":00"
     ncatted -O -a units,time,c,c,"${units}" ${myfile}
     ncatted -O -a calendar,time,c,c,"proleptic_gregorian" ${myfile}
   }

###############################################################################
# 8) main work loop
###############################################################################

   for aFile in $(ls -1 *.nc)
   do
      # make a name with a nice timestamp
     woYear=${aFile/year/}
     woMonth=${woYear/month/}
     woDay=${woMonth/day/}
     woHour=${woDay/hour/}
     workFile=mozart_${woHour}_tmp

     mv ${aFile} ${workFile}

     ###################################################
     # ncap2 -s 'where(lon>180) lon=lon-360' in.nc out.nc 
     ###################################################
 
     # get all variables with spatial reference
     #dreidvar=$(ncdump -h ${workFile} | grep -o "[a-zA-Z0-9\-\_]\{1,50\}(time, lev, lat, lon)")
     #zweidvar=$(ncdump -h ${workFile} | grep -o "[a-zA-Z0-9\-\_]\{1,50\}(time, lat, lon)")

     #dreidvar=${dreidvar//(time, lev, lat, lon)/}
     #zweidvar=${zweidvar//(time, lat, lon)/}

     #dreidvar=${dreidvar//[[:space:]]/,}
     #zweidvar=${zweidvar//[[:space:]]/,}

     # INT2COSMO builtin interpolation routine cannot handle the wrap around at 360 / 0 deg.

     # Only way found so far: use cdo sellonlatbox (Andrew Ferrone). However - this script
     # cannot be used if any non-spatial variables are found in the file. Hence we need
     # to cut them out, process, re-add...

     #ncks -v $dreidvar,$zweidvar ${workFile} ${workFile}_tmp
     #cdo -s sellonlatbox,0,180,-90,90 ${workFile}_tmp ${workFile}_tmpE
     #cdo -s sellonlatbox,180.1,360,-90,90 ${workFile}_tmp ${workFile}_tmpW
     #ncap -s "lon=lon-180" ${workFile} ${workFile}2
     
     #ncks -h -C -A ${workFile}_tmp2 ${workFile}2 

     #rm ${workFile}_tmp ${workFile}_tmp2
     #mv ${workFile}2 ${workFile}

     # INT2LM assumes surface pressure to be named PSURF
     ncrename -O -v PS,PSURF ${workFile}
 
    # remove _VMR_inst from tracer name
    vmrinstvar=$(ncdump -h ${workFile} | grep -o "[a-zA-Z0-9\-\_]\{1,50\}_VMR_inst(time")
    vmrinstvar=${vmrinstvar//(time/}
    #local renameCall=""
    for spec in $vmrinstvar
    do
         ncrename -O -v ${spec},${spec//_VMR_inst/} ${workFile}
    done
    

     # get the correct ozone tracer
     #ncdump -h mozart_${woHour}_tmp | grep -q "O3RAD"
     #local hasO3RAD=$?

     #if [[ $hasO3RAD -eq 0 ]]
     #then
     #   In case of MOZART-NCEP, the producers of the dataset give advice to use O3RAD instead of O3 (<- SYNOZ)
     #  ncrename -O -v O3RAD,OZONE ${workFile}
     #else
     #   MOZART-GEOS does not have this problem
     #  ncrename -O -v O3,OZONE ${workFile}
     #fi

     # using the matching from table 7 in Emmons et al., 2010, Geosc. Model. Dev.
     ncrename -O -v O3,OZONE         ${workFile}
     ncrename -O -v C2H6,ETH         ${workFile}
     ncrename -O -v BIGALK,HC5       ${workFile}
     ncrename -O -v TOLUENE,TOL      ${workFile}
     ncrename -O -v ISOP,ISO         ${workFile}
     ncrename -O -v HO2NO2,HNO4      ${workFile}
     ncrename -O -v CH2O,HCHO        ${workFile}
     ncrename -O -v SO4,VSO4Jm       ${workFile}
     ncrename -O -v NH4,VNH4Jm       ${workFile}
     ncrename -O -v DUST1,VSOILA     ${workFile}
     ncrename -O -v DUST2,VSOILB     ${workFile}
     ncrename -O -v DUST3,VSOILC     ${workFile}
     ncrename -O -v C3H8,HC3         ${workFile}
     ncrename -O -v C2H4,OL2         ${workFile}
     ncrename -O -v CH3COOH,ORA2     ${workFile}
     ncrename -O -v GLYOXAL,GLY      ${workFile}
     ncrename -O -v CH3OOH,OP1       ${workFile}
     ncrename -O -v C2H5OOH,OP2      ${workFile}
     ncrename -O -v CH3COOOH,PAA     ${workFile}
     ncrename -O -v CH3COCHO,MGLY    ${workFile}
     ncrename -O -v BIGENE,OLET      ${workFile}
     ncrename -O -v CRESOL,CSL       ${workFile}

    # ALD is CH3CHO and GLYALD
    ncap -O -s ALD=CH3CHO+GLYALD mozart_${woHour}_tmp ${workFile}
    ncatted -O -a units,ALD,a,c,"VMR" ${workFile}

    # KET is CH3COCH3 and HYAC and MEK
    ncap -O -s KET=CH3COCH3+HYAC+MEK ${workFile} ${workFile}
    ncatted -O -a units,KET,a,c,"VMR" ${workFile}

    # MACR is MVK and MACR
    ncap -O -s MACR=MVK+MACR ${workFile} ${workFile}
    ncatted -O -a units,MACR,a,c,"VMR" ${workFile}
 
    # VSOOTJ is CB1 and CB2
    ncap -O -s VSOOTJ=CB1+CB2 ${workFile} ${workFile}
    ncatted -O -a units,VSOOTJ,a,c,"mol/mol" ${workFile}

    # VORG1Jm is OC1 and OC2
    ncap -O -s VORG1Jm=OC1+OC2 ${workFile} ${workFile}
    ncatted -O -a units,VORG1Jm,a,c,"mol/mol" ${workFile}

    # change unit of aerosols from VMR to mol/mol
    ncatted -O -a units,VSO4Jm,m,c,"mol/mol" ${workFile}
    ncatted -O -a units,VNH4Jm,m,c,"mol/mol" ${workFile}
    ncatted -O -a units,VORG1Jm,m,c,"mol/mol" ${workFile}
    ncatted -O -a units,VSOOTJ,m,c,"mol/mol" ${workFile}

    outVar="date,datesec,T,PSURF,OZONE,NO,NO2,NO3,HNO3,OH,ONIT,H2O2,HO2,HNO4,N2O5,NH3,"
    outVar=${outVar}"ALD,HCHO,CO,ETH,PAN,MPAN,OL2,ORA2,GLY,OP1,OP2,PAA,MGLY,CSL,HC5,HC3,ISO,TOL,KET,MACR,SO2,"
    outVar=${outVar}"VSOILA,VSOILB,VSOILC,VSOOTJ,VORG1Jm,VSO4Jm,VNH4Jm"
    #  local outVar=${outVar}"VSOILA0,VSOILB0,VSOILC0,VAC0m"

     # cleanup step: only put those variables in the output file we can really use:
     ncks -c -v ${outVar} ${workFile} ${workFile}1
     mv ${workFile}1 ${workFile}

     # re-add the coordinate variables cdo did not copy
     ncks -A -v P0,hyam,hybm ${mozartFile} ${workFile}
     # rename variable lev to level (COSMO convention...)
     ncrename -O -d lev,level -v lev,level ${workFile} ${workFile}
     # recalculate the date...
     chdate ${workFile}

     mv ${workFile} mozart_${woHour}
   done

   cd ${workDir}

   # move files from temporary to output directory
   mv -f $mozartNcepTmpDir/mozart_??????????.nc $outputPath
   rm -rf $mozartNcepTmpDir
   rm *_LonAdjust.nc
   # all done, have a nice day!
