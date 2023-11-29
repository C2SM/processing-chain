#!/usr/bin/env python
# -*- coding: utf-8 -*-


def check_model(cfg, model='COSMO'):
    """Check that the model specified in cfg matched the prescribed model.

    Check that cfg.model == model. If not, raises a value-error.
    Ignores capitalization of the strings

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.

    model : str
        Prescribed model
    """
    #don't care about capitalization
    if not cfg.model.lower() == model.lower():
        raise ValueError("The model specified in the configuration file is {}"
                         ", but the job only applies to {}.".format(
                             cfg.model, model))
