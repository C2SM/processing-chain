def vprmsplit(date,ipath,opath):

    import sys
    import numpy as np
    from netCDF4 import Dataset

    m_co2 = 44.01

#    date=str(sys.argv[1])
#    ipath=str(sys.argv[2])
#    opath=str(sys.argv[3])

    ifile_gee = Dataset(ipath+'/VPRM_ECMWF_GEE_'+date[:4]+'.nc', mode='r')
    ifile_resp = Dataset(ipath+'/VPRM_ECMWF_RESP_'+date[:4]+'.nc', mode='r')
    lat = ifile_gee.variables['lat'][:]
    lon = ifile_gee.variables['lon'][:]
    time = ifile_gee.variables['time'][:]
    gee = ifile_gee.variables['GEE'][:]
    resp = ifile_resp.variables['RESP'][:]
    ifile_gee.close()
    ifile_resp.close()

    gpp = gee - resp

    gpp *=1e-9*m_co2
    gpp *= -1.0
    gpp[gpp<0.0] = 0.0

    resp *=1e-9*m_co2

    for ti,tt in enumerate(time):

        dd = int(date[6:8])+int(ti/24.)
        if (dd<=9):
            day='0'+str(dd)
        else:
            day=str(dd)

        hh = int(ti-24*(dd-1))
        if (hh<=9):
            hour='0'+str(hh)
        else:
            hour=str(hh)
        date_ok = date[:6]+day+hour

        ofile_gpp = Dataset(opath+'/gpp_'+date_ok+'.nc', mode='w')

        olat = ofile_gpp.createDimension('lat', len(lat))
        olon = ofile_gpp.createDimension('lon', len(lon))
        otime = ofile_gpp.createDimension('time', 1)


        olat = ofile_gpp.createVariable('lat', np.float64, ('lat',))
        olon = ofile_gpp.createVariable('lon', np.float64, ('lon',))
        otime = ofile_gpp.createVariable('time', np.float64, ('time',))
        otime.units = 'seconds since '+date[:4]+'-'+date[4:6]+'-'+date[6:8]+' 00:00:00'
        otime.calendar = 'proleptic_gregorian'

        ogpp = ofile_gpp.createVariable('CO2_GPP_F', np.float32, ('time','lat','lon'),fill_value=-999.99)
        ogpp.units = 'kg m-2 s-1'
        ogpp.long_name = 'surface upward mass flux of GPP CO2'
        ogpp.standard_name = 'surface_upward_mass_flux_of_gpp_carbon_dioxide'

        olat[:] = lat
        olon[:] = lon
        ogpp[:] = gpp[ti,:]

        ofile_gpp.close()

        ofile_resp = Dataset(opath+'/ra_'+date_ok+'.nc', mode='w')

        olat = ofile_resp.createDimension('lat', len(lat))
        olon = ofile_resp.createDimension('lon', len(lon))
        otime = ofile_resp.createDimension('time', 1)

        olat = ofile_resp.createVariable('lat', np.float64, ('lat',))
        olon = ofile_resp.createVariable('lon', np.float64, ('lon',))
        otime = ofile_resp.createVariable('time', np.float64, ('time',))
        otime.units = 'seconds since '+date[:4]+'-'+date[4:6]+'-'+date[6:8]+' 00:00:00'
        otime.calendar = 'proleptic_gregorian'

        oresp = ofile_resp.createVariable('CO2_RESP_F', np.float32, ('time','lat','lon'),fill_value=-999.99)
        oresp.units = 'kg m-2 s-1'
        oresp.long_name = 'surface upward mass flux of respiration CO2'
        oresp.standard_name = 'surface_upward_mass_flux_of_respiration_carbon_dioxide'

        olat[:] = lat
        olon[:] = lon
        otime[:] = 0.
        oresp[:] = resp[ti,:]

        ofile_resp.close()
