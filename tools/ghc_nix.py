#!/bin/env python

"""Create a .dir-locals.el file for Emacs that uses GHC inside a nix shell."""

import os
from pprint import pprint
import subprocess
import sys


def _run_command(args):
    try:
        return subprocess.check_output(args, shell=True)
    except TypeError as e:
        raise TypeError('%s: %r' % (e, args))


def get_ghc_path():
    return _run_command("which ghc").strip()


def get_packages_databases():
    ghc_pkg_output = _run_command("ghc-pkg list")
    return find_package_databases(ghc_pkg_output)


def find_package_databases(ghc_pkg_output):
    """Given output of `ghc-pkg list`, yield package database paths."""
    # `ghc-pkg list` emits package database paths in the first column,
    # sometimes followed by a colon. Packages are emitted with indentation.
    for line in ghc_pkg_output.splitlines():
        line = line.rstrip()
        if not line:
            continue
        # Hilariously, 'ghc-pkg list' sometimes has a colon after the package
        # database and sometimes does not:
        # https://ghc.haskell.org/trac/ghc/ticket/10785
        if line[-1] == ':':
            yield line[:-1]
        elif line == line.strip():
            yield line


class FileNotFound(Exception):
    """Raised when we can't find a dominating file."""

    def __init__(self, directory, filename):
        super(FileNotFound, self).__init__(
            "%r not found in %r or any directory above it"
            % (filename, directory))
        self.filename = filename
        self.directory = directory


def iter_parents(path):
    """Yield 'path' and all its parent directories."""
    path = os.path.abspath(path)
    parent = os.path.dirname(path)
    while path != parent:
        yield path
        path = parent
        parent = os.path.dirname(path)
    yield parent


def find_dominating_file(start_directory, filename):
    """Look up the tree until we find a directory that contains 'filename'."""
    for directory in iter_parents(start_directory):
        file_path = os.path.join(directory, filename)
        if os.path.exists(file_path):
            return file_path
    raise FileNotFound(start_directory, filename)


def make_dir_locals(ghc_path, package_dbs):
    return '''\
;;; Directory Local Variables
;;;
;;; Automatically generated by ghc_nix.py.
((haskell-mode
  (flycheck-haskell-ghc-executable . "{ghc_path}")
  (flycheck-ghc-package-databases {package_db})))
'''.format(
    ghc_path=ghc_path,
    package_db=' '.join('"{}"'.format(path) for path in package_dbs))


def main(args):
    ghc_path = get_ghc_path()
    package_dbs = get_packages_databases()
    print make_dir_locals(ghc_path, package_dbs)
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))