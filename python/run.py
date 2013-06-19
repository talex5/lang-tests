import os
from os.path import join

import bindings, command
import stores, basedirs
from common import *

def get_digests(elem):
	for child in ZI.children(elem, "manifest-digest"):
		for name, value in child.attrs.items():
			# Non-namespaced attributes only
			if ' ' not in name:
				yield (name, value)
	# Backwards compat...
	impl_id = elem.attrs['id']
	if not impl_id.startswith('/'):
		try:
			yield stores.parse_algorithm_digest_pair(impl_id)
		except stores.BadDigest:
			pass

def ensure_runenv(cache_dir):
	exe = join(cache_dir, 'runenv.py')
	if not os.path.exists(exe):
		mydir = os.path.dirname(os.path.abspath(__file__))
		os.symlink(join(mydir, 'runenv.py'), exe)

class Runner:
	def __init__(self, config):
		self.config = config
		self.stores = stores.Stores(config)
	
	def _get_path(self, elem):
		path = elem.attrs.get("local-path", None)
		if path: return path

		impl_id = elem.attrs["id"]
		if impl_id.startswith('package:'):
			return None

		digests = list(get_digests(elem))
		for digest in digests:
			path = self.stores.lookup(digest)
			if path: return path

		# Backwards compat stuff...

		if impl_id.startswith('/'):
			return impl_id		# Backwards compat

		raise Exception("Item with digests {digests} not found in {search}".format(
			digests = digests,
			search = self.config.cache))
		

	def execute_selections(self, sels, args):
		injector_cache_dir = basedirs.save_path(self.config.cache, join('0install.net', 'injector'))
		ensure_runenv(injector_cache_dir)

		impls = {elem.attrs["interface"]: (elem, self._get_path(elem)) for elem in ZI.children(sels, "selection")}

		bs = bindings.collect_bindings(impls, sels)

		env = os.environ.copy()

		# Environment bindings...
		for (iface, b) in bs:
			if isinstance(b, bindings.EnvironmentBinding):
				(sel, impl_path) = impls[iface]
				val = b.get_value(impl_path, env.get(b.name, None))
				if val is not None:
					#print("{}={}".format(b.name, val))
					env[b.name] = val

		# Executable bindings...
		for (iface, b) in bs:
			if isinstance(b, bindings.ExecutableBinding):
				b.do_exec_binding(self.config, env, impls, iface)

		argv = command.build_command(impls, sels.attrs['interface'], sels.attrs['command'], env) + args
		os.execve(argv[0], argv, env)
