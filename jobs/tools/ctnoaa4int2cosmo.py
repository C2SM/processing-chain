import sys
import numpy as np
from netCDF4 import Dataset
import argparse
import os

half_a=(0,7.367743,210.39389,855.361755,2063.779785,3850.91333,6144.314941,8802.356445,11632.75879,14411.12402,16899.46875,18864.75,20097.40234,20429.86328,19755.10938,18045.18359,15379.80566,12077.44629,8765.053711,6018.019531,3960.291504,1680.640259,713.218079,298.495789,95.636963,0)
half_b=(1,0.99401945,0.97966272,0.95182151,0.90788388,0.84737492,0.77159661,0.68326861,0.58616841,0.48477158,0.38389215,0.28832296,0.2024759,0.13002251,0.07353383,0.03412116,0.01114291,0.00181516,0.00007582,0,0,0,0,0,0,0)


def main(date,indir,outdir,param):
    try:
        os.makedirs(outdir, exist_ok=True)
    except (OSError, PermissionError):
        logging.error("Creating output directory failed")
        raise

    ifile = Dataset(os.path.join(indir,param["prefix"]+"_"+date.strftime('%Y-%m-%d')+'.nc'),'r')
    levs = ifile.variables['level'][:]
    lats = ifile.variables['latitude'][:]
    lons = ifile.variables['longitude'][:]

    co2 = np.squeeze(ifile.variables['co2'][:])
    p_bound = np.squeeze(ifile.variables['pressure'][:])
    ifile.close()

    lats_tf=(lats<=63.)&(lats>=35.)
    lons_tf=(lons<=34.5)&(lons>=-10.5)
    lats=lats[np.logical_and(lats>=35.,lats<=63.)]
    lons=lons[np.logical_and(lons>=-10.5,lons<=34.5)]

    co2=co2[:,:,lats_tf,:][:,:,:,lons_tf]*0.04401/0.02896     # * mmCO2/mmAir    
    p_bound=p_bound[:,:,lats_tf,:][:,:,:,lons_tf]
    p=np.empty(shape=(8,25,15,16)) 
    for i in range(0,24):
        p[:,i,:,:]=(p_bound[:,i,:,:]+p_bound[:,i+1,:,:])/2.
    sp=np.squeeze(p[:,0,:,:])

    hyam=np.empty(len(half_a)-1)
    hybm=np.empty(len(half_a)-1)
    for i in range(len(hyam)-1):
        hyam[i]=(half_a[i]+half_a[i+1])/2.
        hybm[i]=(half_b[i]+half_b[i+1])/2.


    ttt=("00","03","06","09","12","15","18","21")

    for ti,tt in enumerate(ttt):

        with Dataset(os.path.join(outdir,param["suffix"]+"_"+date.strftime('%Y%m%d')+tt+'.nc'), mode='w') as ofile:
        
            olev = ofile.createDimension('level', len(levs))
            olat = ofile.createDimension('lat', len(lats))
            olon = ofile.createDimension('lon', len(lons))
            odate = ofile.createDimension('date', 1)

            olat = ofile.createVariable('lat', np.float64, ('lat',))
            olon = ofile.createVariable('lon', np.float64, ('lon',))
            olev = ofile.createVariable('level', np.float64, ('level',))
            otime = ofile.createVariable('time', np.float64, ('date',))
            odate = ofile.createVariable('date', np.float64, ('date',))

            ohyam = ofile.createVariable('hyam', np.float32, ('level',))
            ohybm = ofile.createVariable('hybm', np.float32, ('level',))

            oco2 = ofile.createVariable('CO2_BG', np.float32, ('date','level','lat','lon'),fill_value=-999.99)
            op = ofile.createVariable('pressure', np.float32, ('date','level','lat','lon'),fill_value=-999.99)
            osp = ofile.createVariable('PSURF', np.float32, ('date','lat','lon'),fill_value=-999.99)
            op0 = ofile.createVariable('P0', np.float32, ('date'),fill_value=-999.99)

            odate.comment = 'time-interval average, centered on times in the date axis'
            odate.long_name = 'UTC dates and times'
            odate.units = 'days since '+date.strftime('%Y%m%d')+' 00:00:00'
        #    otime.dtype = 'double'

            otime.units = 'seconds since '+date.strftime('%Y%m%d')+' 00:00:00'
            otime.calendar = 'proleptic_gregorian'

            olat.standard_name = 'latitude'
            olat.long_name = 'latitude'
            olat.units = 'degree_north'
            olat.axis = 'Y'

            olon.standard_name = 'longitude'
            olon.long_name = 'longitude'
            olon.units = 'degree_east'
            olon.axis = 'X'

            ohyam.long_name = 'hybrid A coefficient at layer midpoints'
            ohyam.units = 'Pa'

            ohybm.long_name = 'hybrid B coefficient at layer midpoints'
            ohybm.units = '1'

            olev.positive = 'up'
            olev.units = 'levels'

            oco2.standard_name = 'mass_fraction_of_carbon_dioxide_in_air'
            oco2.long_name = 'mass mixing ratio of CO2 from outside Europe'
            oco2.units = 'kg kg-1'

            op.long_name = 'pressure_at_center_levels'
            op.units = 'Pa'
            op.standard_name = 'air pressure'

            op0.units = 'Pa'

            osp.cell_methods = 'level:mean'
            osp.units = 'Pa'
            osp.long_name = 'surface pressure'
            osp.table = '128'
            osp.lev = '1'

            olat[:] = lats
            olon[:] = lons
            olev[:] = levs
            odate[:] = 3.*ti/24.
            otime[:] = ti*3*3600

            oco2[:] = co2[ti,:]
            osp[:] = sp[ti,:]
            op[:] = p[ti,:]
            op0[:] = 1.
            ohyam[:] = hyam
            ohybm[:] = hybm


if __name__ == "__main__":    
    parser = argparse.ArgumentParser(description="Process all CarbonTracker files of a single date to a format compatible with int2lm",
         formatter_class=argparse.RawTextHelpFormatter
         )
    parser.add_argument('date',  type=str, help='date in format YYYYMMDD')
    parser.add_argument('param',  help='dictionary of the parameters')
    parser.add_argument('-i', type=str, metavar="indir",
                     help='directory of original CarbonTracker files (default is current path)',
                     default=os.getcwd())
    parser.add_argument('-o', type=str,metavar="outdir",
                        help="output directory of processed files (default is indir/processed)")

    indir = parser.get_default("i")
    parser.set_defaults(o=os.path.join(indir,"processed"))


    args = parser.parse_args()

    print(args.param)
    main(args.date,args.i,args.o,args.param)
        
