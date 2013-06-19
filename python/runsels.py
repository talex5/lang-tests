#!/usr/bin/python2
import sys
from os.path import join

import qdom, run, apps
from basedirs import get_default_config

config = get_default_config()
args = sys.argv[1:]
if not args:
	raise Exception("Syntax: runsels.py [APP | SELECTIONS] [ARGS]")

app_or_sels = args[0]
app_args = args[1:]

app_mgr = apps.AppMgr(config)

app_path = app_mgr.lookup_app(app_or_sels, missing_ok = True)
sels_path = join(app_path, "selections.xml") if app_path is not None else app_or_sels

with open(sels_path, 'rb') as stream:
	sels = qdom.parse(stream)

runner = run.Runner(config)
runner.execute_selections(sels, app_args)
