from os.path import join

import basedirs

class BadDigest(Exception):
	"""Thrown if a digest is invalid (either syntactically or cryptographically)."""
	detail = None

def _validate_pair(value):
	"""@type value: str"""
	if '/' in value or \
	   '\\' in value or \
	   value.startswith('.'):
		raise BadDigest("Invalid digest '{value}'".format(value = value))

def parse_algorithm_digest_pair(src):
	"""Break apart an algorithm/digest into in a tuple.
	Old algorithms use '=' as the separator, while newer ones use '_'.
	@param src: the combined string
	@type src: str
	@return: the parsed values
	@rtype: (str, str)
	@raise BadDigest: if it can't be parsed
	@since: 1.10"""
	_validate_pair(src)
	if src.startswith('sha1=') or src.startswith('sha1new=') or src.startswith('sha256='):
		return src.split('=', 1)
	result = src.split('_', 1)
	if len(result) != 2:
		if '=' in src:
			raise BadDigest("Use '_' not '=' for new algorithms, in {src}".format(src = src))
		raise BadDigest("Can't parse digest {src}".format(src = src))
	return result

def format_algorithm_digest_pair(alg, digest):
	"""The opposite of L{parse_algorithm_digest_pair}.
	The result is suitable for use as a directory name (does not contain '/' characters).
	@type alg: str
	@type digest: str
	@rtype: str
	@raise BadDigest: if the result is invalid
	@since: 1.10"""
	if alg in ('sha1', 'sha1new', 'sha256'):
		result = alg + '=' + digest
	else:
		result = alg + '_' + digest
	_validate_pair(result)
	return result

class Stores:
	def __init__(self, config):
		self.config = config
	
	def lookup(self, digest):
		name = format_algorithm_digest_pair(*digest)
		return basedirs.load_first(self.config.cache, join('0install.net', 'implementations', name))
