#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import subprocess

def main(_, source_file, target_dir, __):
    """Wrapper for mozart2int2lm.sh, assumes the this file is in the same 
       directory as mozart2int2lm.sh. source_file and target_dir are forwarded 
       to the bash script.
    """
    script_loc = os.path.dirname(os.path.realpath(__file__))
    proc = subprocess.run([script_loc + "/mozart2int2lm.sh", source_file, target_dir], stdout=subprocess.PIPE)
    print("Process finished, it had the following output to stdout:")
    print(proc.stdout)
