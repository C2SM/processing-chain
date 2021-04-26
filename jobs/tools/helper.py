from __future__ import division, print_function
import os
import pandas as pd
import numpy as np
from fnmatch import fnmatch
import f90nml
import cartopy.crs as ccrs
# scipy.constants (for easy reference)
# =============================================================================
from scipy.constants import *
# g = 9.80665          (standard acceleration of gravity, units: m s-2)
# R = 8.3144598        (gas constant, units: J mol-1 K-1)
# N_A = 6.02214129e23  (Avogadro constant, units: mol-1)

# ICAO Standard Atmosphere
p0 = physical_constants['standard atmosphere'][0] # pressure, units: Pa
T0 =  288.15                                      # temperature, units: K

# gas constants
R_dryair = 287.053 # (gas constant of dry air)
R_vapor = 461.     # (gas constant for water vapour)


# ratio gast constant of dryair and vapor
epsilon = R_dryair / R_vapor

# lapse rate
GAMMA = -6.5e-3 # K / m

GMR = g / R_dryair

C_P = 1005.0 # J / (kg K)
C_V = 717.0  # J / (kg K)

KAPPA = R_dryair / C_P

# molar mass (kg/m3)
M = {
    'air': 28.97e-3,  # dry air
    'CO':  28.01e-3,
    'CO2': 44.01e-3,
    '14CO2': 45.99307e-3,
    'H2O': 18.01528e-3, 
    'NO2': 46.0055e-3,
    'NO':  30.01e-3,
    'O3':  48.00e-3,
    'SO2': 64.066e-3,
    'CH4': 16.0425e-3,
}

METRIC_PREFIXES = {
    'Y':  1e24, # yotta
    'Z':  1e21, # zetta
    'E':  1e18, # exa
    'P':  1e15, # peta
    'T':  1e12, # tera
    'G':  1e9,  # giga
    'M':  1e6,  # mega
    'k':  1e3,  # kilo
    'h':  1e2,  # hecto
    'da': 1e1,  # deca
    '':   1e0,
    'd':  1e-1, # deci
    'c':  1e-2, # centi
    'm':  1e-3, # milli
    'u':  1e-6, # micro
    'µ':  1e-6, # micro (again)
    'n':  1e-9, # nano
    'p':  1e-12, # pico
    'f':  1e-15, # femto
    'a':  1e-18, # atto
    'z':  1e-21, # zepto
    'y':  1e-24, # yocto
}

# precision
var = {
    "U": 0.001,
    "V": 0.001,
    "W": 0.001,
    "T": 0.01,
    "P": 0.1,
    "PS": 0.1,
    "QV": 1e-8,
    "QC": 1e-8,
    "QI": 1e-8,
    "CLCT": 1e-5,
    "CLC": 1e-5,
    "U_10M": 0.001,
    "V_10M": 0.001,
    "T_2M": 0.01,
    "TD_2M": 0.01,
    "RAIN_GSP": 1e-6,
    "SNOW_GSP": 1e-6,
    "RAIN_CON": 1e-6,
    "SNOW_CON": 1e-6,
    "Z0": 1e-6,
    "T_SO": 0.01,
    "W_SNOW": 1e-6,
    "ALHFL_S": 1e-6,
    "ASHFL_S": 1e-6,
    "ASOB_S": 1e-6,
    "ATHB_S": 1e-6,
    "APAB_S": 1e-6,
    "ALB_RAD": 1e-6,
    "AUMFL_S": 1e-6,
    "AVMFL_S": 1e-6,
    "HPBL": 0.1,
    "ASWDIFU_S": 1e-2,
    "ATHD_S": 1e-2,
    "ASWDIR_S": 1e-2,
    "ASWDIFD_S": 1e-2,
    "CO2_BG": 0.01e-6,
    "CO2_GPP": 0.01e-6,
    "CO2_RA": 0.01e-6,
    "CO2_TOT": 0.01e-6,
    "CO2_SURF": 0.01e-6,
    "CO2_A": 0.01e-6,
    "CO2_BV": 0.01e-6,
    "CO2_BC": 0.01e-6,
    "CO2_B0": 0.01e-6,
    "CO2_B1": 0.01e-6,
    "CO2_B2": 0.01e-6,
    "CO2_T00": 0.01e-6,
    "CO2_T01": 0.01e-6,
    "CO2_T02": 0.01e-6,
    "CO2_T03": 0.01e-6,
    "CO2_T04": 0.01e-6,
    "CO2_T05": 0.01e-6,
    "CO2_T06": 0.01e-6,
    "CO2_T07": 0.01e-6,
    "CO2_JV": 0.01e-6,
    "CO2_JC": 0.01e-6,
    "CO_BG": 0.01e-6,
    "CO_TOT": 0.01e-6,
    "CO_SURF": 0.01e-6,
    "CO_A": 0.01e-6,
    "CO_BV": 0.01e-6,
    "CO_BC": 0.01e-6,
    "CO_B0": 0.01e-6,
    "CO_B1": 0.01e-6,
    "CO_B2": 0.01e-6,
    "CO_JV": 0.01e-6,
    "CO_JC": 0.01e-6,
    "NOX_BG": 0.01e-6,
    "NOX_TOT": 0.01e-6,
    "NOX_SURF": 0.01e-6,
    "NOX_A": 0.01e-6,
    "NOX_BV": 0.01e-6,
    "NOX_BC": 0.01e-6,
    "NOX_B0": 0.01e-6,
    "NOX_B1": 0.01e-6,
    "NOX_B2": 0.01e-6,
    "NOX_R00": 0.01e-6,
    "NOX_R02": 0.01e-6,
    "NOX_R12": 0.01e-6,
    "NOX_R24": 0.01e-6,
    "NOX_JV": 0.01e-6,
    "NOX_JC": 0.01e-6,
    "CO2_NPR": 0.01e-6,
    "CO_NPR": 0.01e-6,
    "NOX_NPR": 0.01e-6
}

ANSI_colors = {"green": '\033[32m', "red": '\033[31m', "yellow": '\033[33m'}



def find_variables_file(alternate_csv_file):    
    # Default path for variables.csv
    dir_path = os.path.dirname(os.path.realpath(__file__))
    csv_file = os.path.join(dir_path, 'variables.csv')
    variables = pd.read_csv(csv_file,
                names=['name','lsd','min_value','max_value'],
                header=0, index_col=0)

    # Check for an extra variables.csv file in the cases folder
    # If a variable is defined in both, this one overwrites the default one
    if os.path.exists(alternate_csv_file):
        variables_bis = pd.read_csv(alternate_csv_file,
                        names=['name','lsd','min_value','max_value'],
                        header=0, index_col=0)
        variables = variables_bis.combine_first(variables)

    return variables



def common_unit(gas):
    """ Returns the commonly used unit of a gas. 

    Parameters
    ----------
    gas : str
        Name of the gas species

    Returns
    -------
    unit : str
        unit of the gas species
    """
    if gas == 'CO2':
        unit = 'ppmv'
    elif gas == '14CO2':
        unit = 'ppmv'
    elif gas == 'CO':
        unit = 'ppbv'
    elif gas == 'CH4':
        unit = 'ppbv'
    elif gas == 'NOX':
        unit = 'ppmv'
    elif gas == 'NO':
        unit = 'ppmv'
    elif gas == 'NO2':
        unit = 'ppmv'
    else:
        return ValueError('Unknown gas %s' % gas)
    
    return unit



def convert_unit(x, from_, to, molar_mass=None, p=p0, T=T0):
    """
    Convert between mole fractions, mixing rations and different
    concentrations.

    This function is still incomplete and needs to be tested
    thoroughly. Please check our results!

    Parameters:
        x:     value that needs to be converted (numerical)
        from_: unit of `x` (e.g. "mol mol-1", "ug/m3", ...)
        to_:   unit to which `x` will be converted.
        molar_mass: name or molar mass value of gas
    """
    x = np.asarray(x)

    from_, from_conversion = unit2quantity(from_)
    to, to_conversion = unit2quantity(to)


    if isinstance(molar_mass, str):
        Mi = M[molar_mass]
    else:
        Mi = molar_mass

    if Mi is None and ((from_ in ['xm', 'cm'] and to not in ['xm', 'cm'])
                       or (from_ not in ['xm', 'cm'] and to in ['xm', 'cm'])):
        raise ValueError('Need molar mass to convert %s to %s but M is "%s"' %
                         (from_, to, Mi))

    # convert to molar fraction (in mol mol-1)
    if from_ in ['xM', 'xV']:
        pass
    elif from_ == 'xm':
        x = x * M['air'] / Mi
    elif from_ == 'cM':
        x = x * R * T / p
    elif from_ == 'cn':
        x = x / N_A * R * T / p
    elif from_ == 'cm':
        x = x / Mi * R * T / p
    else:
        raise ValueError('Cannot convert from "%s"' % from_)

    # convert mole fraction to output unit
    if to in ['xM', 'xV']:
        pass
    elif to == 'xm':
        x = x * Mi / M['air']
    elif to == 'cM':
        x = x * p / (R * T)
    elif to == 'cn':
        x = x * N_A * p / (R * T)
    elif to == 'cm':
        x = x * Mi * p / (R * T)
    else:
        raise ValueError('Cannot convert to "%s"' % to)

    return x * from_conversion / to_conversion



def unit2quantity(unit):
    """
    Get quantitity from unit and conversion factor to convert unit
    to SI units.

    Quantities:
        xM: molar fraction
        xV: volume mixing ratios
        xm: mass mixing ratios
        cM: molar concentration
        cn: number density
        cm: mass concentration
    """
    unit = unit.strip()

    quantity = None
    conv = 1.0

    if ' ' in unit or '/' in unit:

        # split in numerator and denominator
        if '/' in unit:
            n, d = unit.split('/')
        else:
            n, d = unit.split()

        # remove whitespace
        n = n.strip()
        d = d.strip()

        # remove "-" and "1"
        d = d.replace('-', '')
        d = d.replace('1', '')

        # molar or mass concentration or volume mixing ratio
        if d.endswith('m3'):
            conv /= METRIC_PREFIXES[d[:-2]]**3

            if n.endswith('mol'):
                quantity = 'cM'
                conv *= METRIC_PREFIXES[n[:-3]]

            elif n.endswith('g'):
                quantity = 'cm'
                conv *= 1e-3 * METRIC_PREFIXES[n[:-1]]

            elif n.endswith('m3'):
                quantity = 'xV'
                conv *= METRIC_PREFIXES[n[:-2]]

        # mass mixing ratio
        elif n.endswith('g') and d.endswith('g'):
            quantity = 'xm'
            conv *= 1e-3 * METRIC_PREFIXES[n[:-1]]
            conv /= 1e-3 * METRIC_PREFIXES[d[:-1]]

        # molar fraction
        elif n.endswith('mol') and d.endswith('mol'):
            quantity = 'xM'
            conv *= METRIC_PREFIXES[n[:-3]]
            conv /= METRIC_PREFIXES[d[:-3]]

    else:
        # molar fraction
        if fnmatch(unit, 'pp?v'):
            quantity = 'xM'
            conv *= {'m': 1e-6, 'b': 1e-9, 't': 1e-12}[unit[2]]

        # mass mixing ratio
        elif fnmatch(unit, 'pp?m'):
            quantity = 'xm'
            conv *= {'m': 1e-6, 'b': 1e-9, 't': 1e-12}[unit[2]]

        # number density
        elif unit.endswith('m-3'):
            quantity = 'cn'
            conv /= METRIC_PREFIXES[unit[:-3]]**3

    if quantity is None:
        raise ValueError('Failed to parse unit "%s"' % unit)

    return quantity, conv



def rotpole2wgs(rlon, rlat, pollon, pollat, inverse=False):
    """
    Transform rotated pole to WGS84.
    """
    c_in = ccrs.RotatedPole(pollon, pollat)
    c_out = ccrs.PlateCarree()

    if inverse:
        c_in, c_out = c_out, c_in

    if np.ndim(rlon) == 0:
        res = c_out.transform_point(rlon, rlat, c_in)
        return res[0], res[1]
    elif np.ndim(rlon) in [1,2]:
        res = c_out.transform_points(c_in, rlon, rlat)
        return res[...,0], res[...,1]
    else:
        shape = rlon.shape
        res = c_out.transform_points(c_in, rlon.flatten(), rlat.flatten())
        return res[:,0].reshape(shape), res[:,1].reshape(shape)



def datasets_equal(dataset1, dataset2, variables, fixed_precisions=False,
                   verbose=True):
    """Compare the contents of dataset1 and dataset2

    Compare with numpy.isclose whether the two datasets are equal. No check for
    equality (of the values or bitwise of the files) is performed, as numerical
    errors can produce slightly different files for essentially identical
    computations. Rather, the values are compared to absolute and relative
    tolerances, check np.ma.allclose documentation for more detail.
    Only unmasked values are compared.

    If variables is not empty, only the provided variables are compared.

    Parameters
    ----------
    dataset1 : netCDF4.Dataset
    dataset2 : netCDF4.Dataset
    variables : list of str
        List of the variables to be compared. If it is empty, all variables
        are compared.
    fixed_precisions : bool
        If True, precision tolerances defined in precisions.py will be used
    verbose : bool
        If True, results will be printed to stdout.
    Returns
    -------
    bool
        True if the datasets, or if provided the selected variables, are equal,
        False otherwise.
    """
    if not variables:
        variables = set(dataset1.variables.keys())
        variables2 = set(dataset2.variables.keys())

        if not variables == variables2:
            ccprint("Files don't contain the same variables.", "red", verbose)
            ccprint("The following variables are in only "
                    "one of the files:", None, verbose)
            ccprint(variables.symmetric_difference(variables2), None, verbose)
            ccprint("The common variables are:", None, verbose)
            ccprint(variables.intersection(variables2), None, verbose)
            return False, False
    else:
        assert set(dataset1.variables.keys()).issuperset(variables), (
            "Dataset 1 doesn't contain all variables that should be compared")
        assert set(dataset2.variables.keys()).issuperset(variables), (
            "Dataset 2 doesn't contain all variables that should be compared")

    result = True
    result_numeric = True

    for var in variables:
        if not dataset1[var].dtype == dataset2[var].dtype:
            ccprint("{} has different types ({}, {}).".format(var,
                    dataset1[var].dtype,dataset2[var].dtype), "red", verbose)
            result = False

        # Compare with pre-defined absolute precision value if
        # precision flag is set, the variable exists in the list and
        # it is a compressed variable (i.e., types are different)
        if fixed_precisions and var in precision.var and not result:
            try:
                if np.ma.allclose(dataset1[var][:], dataset2[var][:],
                                  atol=precision.var[var], rtol=0.0):
                 ccprint("{} is equal within pre-defined tolerance ({})."
                         .format(var, precision.var[var]), "green", verbose)
                else:
                    ccprint("{} is not equal within pre-defined tolerance ({})"
                            .format(var, precision.var[var]), "red", verbose)
                    result = False
                    result_numeric = False
            except TypeError:
                ccprint("{} is not a numeric type ({}) "
                        "and not compared.".format(var, dataset1[var].dtype),
                        None, verbose)
        # Compare with standard absolute tolerance value (1e-8) or with
        # scale_factor from compression
        else:
            try:
                abs_tol = 1e-8
                if 'scale_factor' in dataset1[var].ncattrs():
                    abs_tol = dataset1[var].scale_factor
                if 'scale_factor' in dataset2[var].ncattrs():
                    abs_tol = dataset2[var].scale_factor
                if np.ma.allclose(dataset2[var][:], dataset1[var][:],
                                  atol=abs_tol, rtol=0.0):
                    ccprint("{} is equal within tolerance ({}).".format(var,
                         abs_tol), "green", verbose)

                else:
                    ccprint("{} is not equal".format(var), "red", verbose)
                    result = False
                    result_numeric = False
            except TypeError:
               ccprint("{} is not a numeric type ({}) "
                       "and not compared.".format(var, dataset1[var].dtype),
                       None, verbose)

    return result, result_numeric



def ccprint(text, color=None, verbose=True):
    """Print-wrapper that Conditionally prints Colored text

    Parameters
    ----------
    text : str
        Text to print
    color : str
        Color of the text
        One of 'red','green' or 'yellow', or 'None' for standard
    verbose : bool
        If 'True' the text is printed, if 'False' nothing happens
    """
    if not verbose:
        return

    if color is not None:
        try:
            print(ANSI_colors[color] + text + '\033[0m')
        except KeyError:
            raise ValueError("Unrecognized color")
    else:
        print(text)


def calculate_mair(p, ps, h, q=None):
    """\
    Calculate layer column density of air (kg/m2). If pressure levels `p` are
    for moist air, it computes the density of moist air. The dry air density can
    be computed by providing the specific humdity `q` (kg/kg). Note that COSMO
    provides pressure levels for moist air.
    
    Parameters:
        p:  pressure levels (Pa)
        ps: surface pressure (Pa)
        h:  altitude levels (m)
        q:  specific humidity (kg water vapor per kg moist air)
    """
    if q is None:
        q = np.zeros_like(p)

    if np.ndim(p) == 4:
        p = p[0]
        h = h[0]
        ps = ps[0]

        if q is not None:
            q = q[0]

    # swap vertical axis
    p = p[::-1,:,:]
    h = h[::-1,:,:]

    if q is not None:
        q = q[::-1,:,:]

    ke, je, ie = p.shape
    pp = np.zeros((ke+1,je,ie))
    pp[0] = ps

    h_mid = 0.5 * (h[1:,:,:] + h[:-1,:,:])

    for i, j in np.ndindex((ie, je)):
        pp[1:,j,i] = np.interp(h[1:,j,i], h_mid[:,j,i], p[:,j,i])

    if q is not None:
        mair = - np.diff(pp, axis=0) / g * (1.0 - q)
    else:
        mair = - np.diff(pp, axis=0) / g 

    return mair[::-1,:,:]



def calculate_xgas(xm, mair, gas, q=0.0):
    """\
    Compute column-averaged dry air mole fraction of gas in ppmv.

    Parameters:
        xm:   mass mixing ratio of tracer wrt. moist air (kg/kg)
        mair: layer (moist) air column density (kg m-2)
        gas:  name of gas (e.g. NO2, CO2, NOx, ...)
        q:    specific humidity (kg/kg)
    """
    if np.ndim(xm) == 4:
        xm = xm[0]
    if np.ndim(q) == 4:
        q = q[0]

    xgas = np.sum(mair * xm, axis=0) / np.sum(mair * (1.0 - q), axis=0)
    return 1e6 * xgas * M['air'] / M[gas]



class Domain:
    def __init__(self, name, startlon, startlat, stoplon, stoplat,
                 ie=None, je=None, pollon=None, pollat=None):
        """
        to add: dlon, dlat, ie, je
        """
        self.name = name

        self.startlat = startlat
        self.stoplat = stoplat
        self.startlon = startlon
        self.stoplon = stoplon

        self.ie = ie
        self.je = je

        self.rlon = None
        self.rlat = None
        self.lon = None
        self.lat = None

        self.pollon = pollon
        self.pollat = pollat

        self.is_rotpole =  pollon is not None and pollat is not None
        is_grid = self.ie is not None and self.je is not None

        if is_grid:
            self.dlon = (self.stoplon - self.startlon) / (self.ie - 1)
            self.dlat = (self.stoplat - self.startlat) / (self.je - 1)
        else:
            self.dlon, self.dlat = None, None


        if self.is_rotpole:
            self.proj = ccrs.RotatedPole(pole_latitude=pollat,
                                         pole_longitude=pollon)

            if is_grid:
                self.rlon = np.linspace(self.startlon, self.stoplon, self.ie)
                self.rlat = np.linspace(self.startlat, self.stoplat, self.je)

                rlon, rlat = np.meshgrid(self.rlon, self.rlat)
                self.lon, self.lat = rotpole2wgs(rlon, rlat, pollon, pollat)
        else:
            self.proj = ccrs.PlateCarree()

            if is_grid:
                self.lon = np.linspace(self.startlon, self.stoplon, self.ie)
                self.lat = np.linspace(self.startlat, self.stoplat, self.je)


    @property
    def shape(self):
        return self.je, self.ie


    @classmethod
    def from_nml(cls, filename):
        with open(filename) as nml_file:
            nml = f90nml.read(nml_file)

        pollon = nml['lmgrid']['pollon']
        pollat = nml['lmgrid']['pollat']
        startlon = nml['lmgrid']['startlon_tot']
        startlat = nml['lmgrid']['startlat_tot']
        dlon = nml['lmgrid']['dlon']
        dlat = nml['lmgrid']['dlat']
        ie = nml['lmgrid']['ie_tot']
        je = nml['lmgrid']['je_tot']

        stoplat = startlat + (je-1) * dlat
        stoplon = startlon + (ie-1) * dlon

        return cls(filename, startlon, startlat, stoplon, stoplat,
                   ie, je, pollon, pollat)

