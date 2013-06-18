#!/usr/bin/env python

import os, sys, subprocess
from os.path import join
from xml.sax import saxutils
import time

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

expected = b'/fast\n--fastest\n--arg-to-fast\n/prog.fst\n--arg-to-prog\n-X1\n-Y1\n-X2\n-Y2\n-X3\n-Y3\nuser-arg\n--run arg-to-util\n--test arg-to-test-util\n'

def run():
	return subprocess.check_output([launcher, './selections-tmp.xml', 'user-arg'])

n = 10
actual = run()
assert actual == expected, actual
t1 = time.time()
for x in range(n):
	run()
t2 = time.time()

print("%s took %d ms" % (launcher, (t2 - t1) * 1000 / n))
#print(out.decode('utf-8'))
