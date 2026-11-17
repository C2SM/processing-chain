#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import shutil

from . import tools
from .tools.generate_tracers_xml import generate_tracers_xml

BASIC_PYTHON_JOB = False


def main(cfg):
    """
    **Prepare OEM**

    Generates ``tracers.xml`` (the chemtracer definitions for ICON-ART and
    OEM) from the ``tracers`` section of config.yaml, see
    :func:`jobs.tools.generate_tracers_xml.generate_tracers_xml`. The file is
    written to ``<case_root>/global_inputs/XML/tracers.xml`` and used by all
    chunks, so this job only does something in the first chunk.

    Runs directly on the login node.

    Parameters
    ----------
    cfg : Config
        Object holding all user-configuration parameters as attributes.
    """
    tools.change_logfile(cfg.logfile)

    if cfg.startdate_sim != cfg.startdate:
        logging.info("Not the first chunk, skipping prepare_oem")
        shutil.copy(cfg.logfile, cfg.logfile_finish)
        return

    logging.info("Prepare OEM inputs")
    tools.create_dir(xml_folder := cfg.case_root / "global_inputs" / "XML",
                     "XML")
    with open(xml_folder / "tracers.xml", "w", encoding="utf-8") as file:
        file.write(generate_tracers_xml(cfg.tracers))
    logging.info("OK")

    # Jobs that are not BASIC_PYTHON_JOBs mark themselves as finished, so
    # that a re-run of this chunk skips them
    shutil.copy(cfg.logfile, cfg.logfile_finish)
