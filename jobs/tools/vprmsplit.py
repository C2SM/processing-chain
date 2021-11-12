#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Script to extract VPRM emissions for a single day
and to convert the output into an int2lm compatible format.

Authors: Dominik Brunner (brd), Empa, Switzerland
         Michael Jaehn (jae), Empa, Switzerland

History:
    brd, 23 Jul 2013: first implementation
    brd, 20 Aug 2014: modified to handle files with gpp and ra provided
                      separately
    brd, 12 Jul 2015: corrected for grid coordinate shift by +0.5 grid cells
                      (since cell coordinates are given for lower left corner)
                      and added option to specify CO2 name
    brd, 15 Jan 2017: slightly adapted for hypatia and project SmartCarb
    jae, 17 Aug 2018: translated to Python

TODOs:
    - use logging
"""

from __future__ import print_function

import sys
import numpy as np
from netCDF4 import Dataset
from datetime import datetime
from datetime import timedelta
import logging


def main(year, ipath, opath):
    """
    Script to extract VPRM emissions for a single day
    and to convert the output into an int2lm compatible format.
    
    Output:
    The script generates individual 1-hour emission files for the specified
    year::

        gpp_yyyymmddhh.nc
        ra_yyyymmddhh.nc

    Parameters
    ----------
    year : str
        Year (YYYY) of data to be processed
        The original VPRM input file (e.g., VPRM_ECMWF_*_2017.nc for 2017
        fluxes) needs to be present in the input directory 'ipath'
    ipath : str
        Input path where the VPRM input file is located
    opath : str
        Output path where the processed data is going to be written
    """
    m_co2 = 44.01

    filename_gee = ''.join([ipath, '/VPRM_ECMWF_GEE_', year[:4], '.nc'])
    filename_resp = ''.join([ipath, '/VPRM_ECMWF_RESP_', year[:4], '.nc'])

    print('Opening VPRM GEE file', filename_gee)
    ifile_gee = Dataset(filename_gee, mode='r')
    print('Opening VPRM RESP file', filename_resp)
    ifile_resp = Dataset(filename_resp, mode='r')

    lat = ifile_gee.variables['lat'][:]
    lon = ifile_gee.variables['lon'][:]
    hours = ifile_gee.variables['time'][:]
    gpp = ifile_gee.variables['GEE'][:]
    resp = ifile_resp.variables['RESP'][:]
    ifile_gee.close()
    ifile_resp.close()

    # Get grid spacing of VPRM grid
    dx = lat[1] - lat[0]
    dy = lon[1] - lon[0]

    # Change fluxes from umol m-2 s-1 to kg m-2 s-1 for GPP
    gpp *= 1e-9 * m_co2

    # Switch the sign and avoid negative values
    gpp *= -1.0
    gpp[gpp <= 0.0] = 0.0

    # Change fluxes from umol m-2 s-1 to kg m-2 s-1 for RESP
    resp *= 1e-9 * m_co2

    print(hours)
    for ti, hour in enumerate(hours):

        begin_of_year_dt = datetime.strptime(year[:4] + '0101', '%Y%m%d')
        curdate_dt = begin_of_year_dt + timedelta(hours=int(hour))
        curdate_str = datetime.strftime(curdate_dt, '%Y%m%d%H')
        print(curdate_dt)

        ofile_gpp = Dataset(opath + '/gpp_' + curdate_str + '.nc', mode='w')

        olat = ofile_gpp.createDimension('lat', len(lat))
        olon = ofile_gpp.createDimension('lon', len(lon))
        otime = ofile_gpp.createDimension('time', 1)

        olat = ofile_gpp.createVariable('lat', np.float64, ('lat', ))
        olon = ofile_gpp.createVariable('lon', np.float64, ('lon', ))
        otime = ofile_gpp.createVariable('time', np.float64, ('time', ))
        otime.units = ''.join(['seconds since ', year, '-01-01 00:00:00'])
        otime.calendar = 'proleptic_gregorian'

        ogpp = ofile_gpp.createVariable('CO2_GPP_F',
                                        np.float32, ('time', 'lat', 'lon'),
                                        fill_value=-999.99)
        ogpp.units = 'kg m-2 s-1'
        ogpp.long_name = 'surface upward mass flux of GPP CO2'
        ogpp.standard_name = 'surface_upward_mass_flux_of_gpp_carbon_dioxide'

        olat[:] = lat + dx / 2.
        olon[:] = lon + dy / 2.
        otime[:] = int((curdate_dt - begin_of_year_dt).total_seconds())
        ogpp[:] = gpp[ti, :]

        ofile_gpp.close()

        ofile_resp = Dataset(opath + '/ra_' + curdate_str + '.nc', mode='w')

        olat = ofile_resp.createDimension('lat', len(lat))
        olon = ofile_resp.createDimension('lon', len(lon))
        otime = ofile_resp.createDimension('time', 1)

        olat = ofile_resp.createVariable('lat', np.float64, ('lat', ))
        olon = ofile_resp.createVariable('lon', np.float64, ('lon', ))
        otime = ofile_resp.createVariable('time', np.float64, ('time', ))
        otime.units = ''.join(['seconds since ', year, '-01-01 00:00:00'])
        otime.calendar = 'proleptic_gregorian'

        oresp = ofile_resp.createVariable('CO2_RA_F',
                                          np.float32, ('time', 'lat', 'lon'),
                                          fill_value=-999.99)
        oresp.units = 'kg m-2 s-1'
        oresp.long_name = 'surface upward mass flux of respiration CO2'
        oresp.standard_name = 'surface_upward_mass_flux_of_respiration_carbon_dioxide'

        olat[:] = lat + dx / 2.
        olon[:] = lon + dy / 2.
        otime[:] = int((curdate_dt - begin_of_year_dt).total_seconds())
        oresp[:] = resp[ti, :]

        ofile_resp.close()


if __name__ == '__main__':

    year = sys.argv[1]
    ipath = sys.argv[2]
    opath = sys.argv[3]
    main(year, ipath, opath)
