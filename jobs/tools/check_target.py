#!/usr/bin/env python
# -*- coding: utf-8 -*-

def check_target(cfg, target='COSMO'):
    """Check that the target specified in cfg matched the prescribed target.

    Check that cfg.target == target. If not, raises a value-error.
    Ignores capitalization of the strings

    Parameters
    ----------
    cfg : config-object

    target : str
        Prescribed target
    """
    #don't care about capitalization
    if not cfg.target.lower() == target.lower():
        raise ValueError("The target specified in the configuration file is {}"
                         ", but the job only applies to {}.".format(cfg.target,
                                                                    target))
