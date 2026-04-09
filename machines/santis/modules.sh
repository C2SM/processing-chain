#!/usr/bin/env bash
# machines/santis/modules.sh
#
# Activate the user environment required by the Processing Chain on Santis.
# On Santis, software is provided via uenv (user environments) instead of
# the traditional module system.
#
# NOTE: 'uenv start' spawns a new shell, so it cannot be sourced inside an
# existing shell session.  Use this file as a reference or call it directly
# as a wrapper (see setup_env.sh).
#
# Usage – start an interactive session with the environment:
#   uenv start climtools/25.2:v1 --view=climtools
#
# Usage – run a single command inside the environment:
#   uenv run climtools/25.2:v1 --view=climtools -- <command>

UENV_IMAGE="climtools/25.2:v1"
UENV_VIEW="climtools"
