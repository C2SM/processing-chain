

from datetime import datetime, timedelta
import subprocess
from . import cams4int2cosmo
from . import ctnoaa4int2cosmo


def iter_hours(starttime, hstart, hstop, step=1):

    current = starttime + timedelta(hours=hstart)
    stoptime = starttime + timedelta(hours=hstop)

    while current <= stoptime:
        yield current
        current += timedelta(hours=step)

def iter_times(start, end, step):
    current = start
    while current < end:
        yield current
        current += step

def send_mail(address, subject, message=''):
    p = subprocess.Popen(['mail', '-s', '"%s"' % subject, address], stdin=subprocess.PIPE)
    p.stdin.write(message.encode('utf-8'))
    p.stdin.close()


