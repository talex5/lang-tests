#!/usr/bin/env python

import os, sys, subprocess
from os.path import join
from xml.sax import saxutils

from zeroinstall import support

launcher, = sys.argv[1:]

my_dir = os.path.dirname(os.path.abspath(__file__))

cache_injector = join(my_dir, "cache", "0install.net", "injector")
if os.path.exists(cache_injector):
	support.ro_rmtree(cache_injector)

os.environ['XDG_CONFIG_HOME'] = join(my_dir, 'config')
os.environ['XDG_CACHE_HOME'] = join(my_dir, 'cache')
os.environ['XDG_DATA_HOME'] = join(my_dir, 'data')

os.environ['XDG_CONFIG_DIRS'] = ''
os.environ['XDG_CACHE_DIRS'] = ''
os.environ['XDG_DATA_DIRS'] = ''

os.environ['OCAMLRUNPARAM'] = 'b'

os.chdir(my_dir)

with open('selections.xml', 'r') as stream:
	xml = stream.read()
xml = xml.replace('@TESTS', saxutils.escape(my_dir))
with open('selections-tmp.xml', 'w') as stream:
	stream.write(xml)

out = subprocess.check_call([launcher, './selections-tmp.xml', 'user-arg'])
#print(out.decode('utf-8'))
