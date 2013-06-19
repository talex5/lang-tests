import os
from os.path import join, expanduser
from collections import namedtuple

Basedirs = namedtuple("Basedirs", ["data", "cache", "config"])

def get_default_config():
	def get_path(home_var, dirs_var, defaults):
		home = os.environ.get(home_var, defaults[0])
		dirs = os.environ.get(dirs_var, None)
		if dirs:
			dirs = dirs.split(os.pathsep)
		else:
			dirs = defaults[1:]
		return [home] + dirs

	return Basedirs(
		data = get_path("XDG_DATA_HOME", "XDG_DATA_DIRS", [expanduser("~/.local/share"), "/usr/local/share", "/usr/share"]),
		cache = get_path("XDG_CACHE_HOME", "XDG_CACHE_DIRS", [expanduser("~/.cache"), "/var/cache"]),
		config = get_path("XDG_CONFIG_HOME", "XDG_CONFIG_DIRS", [expanduser("~/.config"), "/etc/xdg"])
	)

def load_first(search_path, rel_path):
	for p in search_path:
		path = join(p, rel_path)
		if os.path.exists(path):
			return path
	return None

def save_path(search_path, rel_path):
	path = join(search_path[0], rel_path)
	if not os.path.isdir(path):
		os.makedirs(path, 0o700)
	return path
