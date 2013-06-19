#!/usr/bin/python2
import os, sys, json

envname = os.path.basename(sys.argv[0])
args = json.loads(os.environ["0install-runenv-" + envname])
os.execv(args[0], args + sys.argv[1:])
