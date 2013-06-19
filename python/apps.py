from os.path import join
import re
import basedirs

valid_name = re.compile(r'''^[^./\\:=;'"][^/\\:=;'"]*$''')

def validate_name(name):
	if valid_name.match(name): return
	raise Exception("Invalid application name '{name}'".format(name = name))

class AppMgr:
	def __init__(self, config):
		self.config = config

	def lookup_app(self, name, missing_ok = True):
		if not valid_name.match(name):
			if missing_ok:
				return None
			else:
				raise SafeException("Invalid application name '{name}'".format(name = name))
		app_dir = basedirs.load_first(self.config.config, join("0install.net", "apps", name))
		if app_dir:
			return app_dir
		if missing_ok:
			return None
		else:
			raise SafeException("No such application '{name}'".format(name = name))
