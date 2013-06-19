import os
from string import Template

from common import *

def get_command(sel, name):
	for x in ZI.children(sel, "command"):
		if x.attrs['name'] == name:
			return x
	raise Exception("Missing <command>")

def get_runner(command):
	for x in ZI.children(command, "runner"):
		return x
	return None

def _process_args(args, element, env):
	"""Append each <arg> under <element> to args, performing $-expansion. Also, process <for-each> loops."""
	for child in element.childNodes:
		if child.uri != ZI.ns: continue

		if child.name == 'arg':
			args.append(Template(child.content).substitute(env))
		elif child.name == 'for-each':
			array_var = child.attrs['item-from']
			separator = child.attrs.get('separator', os.pathsep)
			env_copy = env.copy()
			seq = env.get(array_var, None)
			if seq:
				for item in seq.split(separator):
					env_copy['item'] = item
					_process_args(args, child, env_copy)

def build_command(impls, command_iface, command_name, env):
	assert command_name is not None, "Can't run: no command specified!"

	prog_args = []

	while command_name:
		(command_sel, command_sel_path) = impls[command_iface]

		command = get_command(command_sel, command_name)

		command_args = []

		# Add extra arguments for runner
		runner = get_runner(command)
		if runner:
			command_iface = runner.attrs['interface']
			command_name = runner.attrs.get('command', 'run')
			_process_args(command_args, runner, env)
		else:
			command_iface = None
			command_name = None

		# Add main program path
		command_path = command.attrs.get('path', None)
		if command_path is not None:
			if command_sel_path is None:
				prog_path = command_path
			else:
				if command_path.startswith('/'):
					raise SafeException(_("Command path must be relative, but '%s' starts with '/'!") %
								command_path)
				prog_path = os.path.join(command_sel_path, command_path)

			assert prog_path is not None

			if not os.path.exists(prog_path):
				raise Exception(_("File '%(program_path)s' does not exist.\n"
						"(implementation '%(implementation_id)s' + program '%(main)s')") %
						{'program_path': prog_path, 'implementation_id': command_sel.attrs['id'],
						'main': command_path})

			command_args.append(prog_path)

		# Add extra arguments for program
		_process_args(command_args, command, env)

		prog_args = command_args + prog_args

	# Each command is run by the next, but the last one is run by exec, and we
	# need a path for that.
	if command_path is None:
		raise SafeException("Missing 'path' attribute on <command>")

	return prog_args
