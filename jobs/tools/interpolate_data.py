import xarray as xr
import numpy as np
import os
import scipy

from .constants import *

def vertical_interp(pres_in, pres_out):
    nvertin, nptsin = pres_in.shape
    nvertout, nptsout = pres_out.shape

    # Initializing index mesh for scipy.griddata
    indexin = np.array((nvertin + 2) * [list(range(nptsin))])
    indexout = np.array(nvertout * [list(range(nptsout))])

    levmeshin = np.array(nptsin * [list(range(-1, nvertin + 1))]).T
    levin = levmeshin.flatten()

    eps = max(np.ptp(pres_in), np.ptp(pres_out)) / min(nvertin, nvertout) / 10
    pin = np.concatenate(
        [
            max(pres_in.max(), pres_out.max()) * np.ones((1, nptsin)) + eps,
            pres_in,
            min(pres_in.min(), pres_out.min()) * np.ones((1, nptsout)) - eps,
        ],
        axis=0,
    ).flatten()
    pout = pres_out.flatten()

    # Aggegating points in scipy.griddata format
    points_in = np.array([indexin.flatten(), pin]).T
    points_out = np.array([indexout.flatten(), pout]).T

    # Retrieving location of ak levels versus model levels
    levout = scipy.interpolate.griddata(points_in, levin, points_out)
    levout = levout.reshape((-1, nptsout))

    # Getting interpolation coefficients
    xlow = np.floor(levout).astype(int)
    xhigh = xlow + 1

    alphahigh = levout - xlow
    alphalow = 1 - alphahigh

    # Cropping outside pres_in
    toolow = xlow <= 0
    toohigh = xhigh >= nvertin - 1

    xlow[toolow] = 0
    xhigh[toohigh] = nvertin - 1

    alphalow[toolow] = 1
    alphahigh[toolow] = 0
    alphalow[toohigh] = 0
    alphahigh[toohigh] = 1

    return xlow, xhigh, alphalow, alphahigh


def compute_pmid_era5(ds_era):

    temp = ds_era['T']
    psurf = ds_era['PS']
    ap = ds_era['hyai']
    bp = ds_era['hybi']
    skelet = xr.ones_like(temp)
    skelet_alt = skelet.isel(time=0, ncells=0)
    ap_bot = xr.ones_like(skelet) * (ap[:-1].values * skelet_alt)
    bp_bot = xr.ones_like(skelet) * (bp[:-1].values * skelet_alt)
    ap_top = xr.ones_like(skelet) * (ap[1:].values * skelet_alt)
    bp_top = xr.ones_like(skelet) * (bp[1:].values * skelet_alt)

    pbot = ap_bot + bp_bot * psurf
    ptop = ap_top + bp_top * psurf

    pmid = 0.5 * (pbot + ptop)

    return pmid


def compute_pmid_transcom(field, ap, bp, psurf):
    skelet = xr.DataArray(np.ones(field.shape),
                        dims=['LEVEL', 'ncells'],
                        coords=[field['LEVEL'], np.arange(5120)])
    skelet_alt = skelet.isel(ncells=0)
    ap_bot = xr.ones_like(skelet) * (ap[:-1].values * skelet_alt)
    bp_bot = xr.ones_like(skelet) * (bp[:-1].values * skelet_alt)
    ap_top = xr.ones_like(skelet) * (ap[1:].values * skelet_alt)
    bp_top = xr.ones_like(skelet) * (bp[1:].values * skelet_alt)

    pbot = ap_bot + bp_bot * psurf
    ptop = ap_top + bp_top * psurf

    pmid = 0.5 * (pbot + ptop)

    return pmid

def create_oh_for_restart(cfg, month, ext_restart):

        ds_restart = xr.open_dataset(cfg.restart_filename_scratch)
        nlevels = 65

        ds = xr.open_dataset(cfg.OH_MOLEC_FILENAME)
        oh = ds['OH'][month - 1]
        pres_surf = ds['PSURF'][month - 1]
        pmid_transcom = compute_pmid_transcom(oh, ds['A'], ds['B'], pres_surf)

        exner_icon = ds_restart['exner' + ext_restart]
        rho_icon = ds_restart['rho' + ext_restart]
        qv_icon = ds_restart['qv' + ext_restart]
        pmid_icon = 100000 * np.power(exner_icon, 3.5)

        oh_regrid = xr.zeros_like(pmid_icon)
        oh_regrid['cells'] = np.arange(len(oh_regrid.cells))

        oh_regrid = oh_regrid.stack(stack=["time", "cells"])
        oh = oh.stack(stack=["ncells"])
        pmid_icon = pmid_icon.stack(stack=["time", "cells"])
        pmid_transcom = pmid_transcom.stack(stack=["ncells"])

        nchunks = 20
        npixels = pmid_transcom.shape[1]
        chunks = np.linspace(0, npixels, num=nchunks, dtype=int)
        for i, (k1, k2) in enumerate(zip(chunks[:-1], chunks[1:])):
            pres_in = np.log(pmid_transcom[:, k1:k2])
            pres_out = np.log(pmid_icon[:, k1:k2])

            xlow, xhigh, alphalow, alphahigh = vertical_interp(pres_in.values, pres_out.values)
            ycoord = np.array(nlevels * [list(range(k2 - k1))])
            oh_chunk = oh.values[:, k1:k2]
            oh_regrid[:, k1:k2] = alphalow * oh_chunk[xlow, ycoord] + alphahigh * oh_chunk[xhigh, ycoord]

        oh_regrid = oh_regrid.unstack()
        oh_regrid = oh_regrid.transpose('time', 'layers_{}'.format(nlevels), 'cells')

        M_moistair = M_DRYAIR * M_WATER / (qv_icon * M_DRYAIR + (1 - qv_icon) * M_WATER)
        oh_regrid = oh_regrid * M_moistair.data / (N_AVO * 1e-6 * rho_icon.data)

        ds_restart['TROH' + ext_restart] = exner_icon.copy()
        ds_restart['TROH' + ext_restart][:] = oh_regrid.data

        os.remove(cfg.restart_filename_scratch)
        ds_restart.to_netcdf(cfg.restart_filename_scratch)


def create_oh_for_inicond(cfg, month):

        ds_inicond = xr.open_dataset(cfg.inicond_filename_scratch)
        pmid_era5 = compute_pmid_era5(ds_inicond)
        temp_era5 = ds_inicond['T']
        nlevels = len(temp_era5.lev)

        ds = xr.open_dataset(cfg.OH_MOLEC_FILENAME)
        oh = ds['OH'][month - 1]
        pres_surf = ds['PSURF'][month - 1]
        pmid_transcom = compute_pmid_transcom(oh, ds['A'], ds['B'], pres_surf)

        oh_regrid = xr.zeros_like(pmid_era5)
        oh_regrid['ncells'] = np.arange(len(oh_regrid.ncells))

        oh_regrid = oh_regrid.stack(stack=["time", "ncells"])
        oh = oh.stack(stack=["ncells"])
        pmid_era5 = pmid_era5.stack(stack=["time", "ncells"])
        pmid_transcom = pmid_transcom.stack(stack=["ncells"])
        
        nchunks = 20
        npixels = pmid_transcom.shape[1]
        chunks = np.linspace(0, npixels, num=nchunks, dtype=int)
        for i, (k1, k2) in enumerate(zip(chunks[:-1], chunks[1:])):
            pres_in = np.log(pmid_transcom[:, k1:k2])
            pres_out = np.log(pmid_era5[:, k1:k2])

            xlow, xhigh, alphalow, alphahigh = vertical_interp(pres_in.values, pres_out.values)
            ycoord = np.array(nlevels * [list(range(k2 - k1))])
            oh_chunk = oh.values[:, k1:k2]
            oh_regrid[:, k1:k2] = alphalow * oh_chunk[xlow, ycoord] + alphahigh * oh_chunk[xhigh, ycoord]

        oh_regrid = oh_regrid.unstack()
        oh_regrid = oh_regrid.transpose('time', 'lev', 'ncells')
        # oh_regrid = oh_regrid * temp_era5.data * R_GAS / (N_AVO * 1e-6 * pmid_era5.data)
        oh_regrid = oh_regrid * temp_era5.data * R_GAS / (N_AVO * 1e-6 * pmid_era5.data)

        pmid_era5 = pmid_era5.unstack()
        pmid_era5 = pmid_era5.transpose('time', 'lev', 'ncells')
        ds_inicond['TROH'] = pmid_era5.copy()
        ds_inicond['TROH'][:] = oh_regrid.data

        os.remove(cfg.inicond_filename_scratch)
        ds_inicond.to_netcdf(cfg.inicond_filename_scratch)