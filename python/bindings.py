import os, json
from os.path import join

import basedirs, command
from common import *

binding_elements = {'environment', 'executable-in-var', 'executable-in-path'}
dep_elements = {'restricts', 'requires', 'runner'}

def validate_exec_name(name):
	if '/' in name or name.startswith('.') or "'" in name:
		raise SafeException("Invalid <executable> name '%s'" % name)

class EnvironmentBinding:
	"""Indicate the chosen implementation using an environment variable."""
	PREPEND = 'prepend'
	APPEND = 'append'
	REPLACE = 'replace'

	defaults = {
		'PATH': '/bin:/usr/bin',
		'XDG_CONFIG_DIRS': '/etc/xdg',
		'XDG_DATA_DIRS': '/usr/local/share:/usr/share',
	}

	def __init__(self, name, insert, default = None, mode = PREPEND, value=None, separator=None):
		"""
		mode argument added in version 0.28
		value argument added in version 0.52
		"""
		self.name = name
		self.insert = insert
		self.default = default
		self.mode = mode
		self.value = value
		if separator is None:
			self.separator = os.pathsep
		else:
			self.separator = separator


	def __str__(self):
		return ("<environ %(name)s %(mode)s %(insert)s %(value)s>") % \
			{'name': self.name, 'mode': self.mode, 'insert': self.insert, 'value': self.value}

	__repr__ = __str__

	def get_value(self, path, old_value):
		"""Calculate the new value of the environment variable after applying this binding.
		@param path: the path to the selected implementation
		@param old_value: the current value of the environment variable
		@return: the new value for the environment variable"""

		if self.insert is not None:
			if path is None:
				return None
			extra = os.path.join(path, self.insert)
		else:
			assert self.value is not None
			extra = self.value

		if self.mode == EnvironmentBinding.REPLACE:
			return extra

		if old_value is None:
			old_value = self.default
			if old_value is None:
				old_value = EnvironmentBinding.defaults.get(self.name, None)
		if old_value is None:
			return extra
		if self.mode == EnvironmentBinding.PREPEND:
			return extra + self.separator + old_value
		else:
			return old_value + self.separator + extra

class ExecutableBinding:
	IN_PATH = "path"
	IN_VAR = "var"

	def __init__(self, exec_type, name, command):
		validate_exec_name(name)
		assert command

		self.exec_type = exec_type
		self.name = name
		self.command = command
	
	def do_exec_binding(self, config, env, impls, iface):
		exec_dir = basedirs.save_path(config.cache, join("0install.net", "injector", "executables", self.name))
		exec_path = join(exec_dir, self.name)
		if not os.path.exists(exec_path):
			os.symlink('../../runenv.py', exec_path)
			os.chmod(exec_dir, 0o500)

		command_argv = command.build_command(impls, iface, self.command, env)
		if self.exec_type == ExecutableBinding.IN_PATH:
			old = env.get('PATH', EnvironmentBinding.defaults['PATH'])
			env['PATH'] = exec_dir + os.pathsep + old
		else:
			env[self.name] = exec_path

		env['0install-runenv-' + self.name] = json.dumps(command_argv)

modes = {
	'prepend': EnvironmentBinding.PREPEND,
	'append': EnvironmentBinding.APPEND,
	'replace': EnvironmentBinding.REPLACE,
}

def collect_bindings(impls, sels):
	bindings = []
	def process(elem, iface, valid_children):
		for child in elem.childNodes:
			if child.uri != ZI.ns: continue
			if child.name not in valid_children: continue

			if child.name == 'command':
				process(child, iface, binding_elements | dep_elements)
			elif child.name in dep_elements:
				dep_iface = child.attrs["interface"]
				if dep_iface in impls:
					process(child, dep_iface, binding_elements)
			elif child.name == 'environment':
				a = child.attrs
				bindings.append((iface, EnvironmentBinding(
					name = a['name'],
					insert = a.get('insert', None),
					default = a.get('default', None),
					mode = modes[a.get('mode', 'prepend')],
					value = a.get('value', None),
					separator = a.get('separator', None)
				)))
			elif child.name in ('executable-in-var', 'executable-in-path'):
				a = child.attrs
				bindings.append((iface, ExecutableBinding(
					ExecutableBinding.IN_VAR if child.name == 'executable-in-var' else ExecutableBinding.IN_PATH,
					a['name'],
					command = a.get('command', 'run')
				)))
			else:
				raise Exception(child.name)
	for sel in ZI.children(sels, "selection"):
		process(sel, sel.attrs["interface"], {'command'} | binding_elements | dep_elements)
	return bindings

