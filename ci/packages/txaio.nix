{ stdenv, buildPythonPackage, fetchurl, pythonPackages }:

assert pythonPackages.isPy27;

buildPythonPackage rec {
  name = "txaio-${version}";
  version = "2.3.1";

  src = fetchurl {
    url = "https://pypi.python.org/packages/source/t/txaio/${name}.tar.gz";
    sha256 = "0rxbmf77c7jyna1igll5qi8zdql1fwkbp31icwh2rqkkzxxwh48i";
  };

  propagatedBuildInputs =
    with pythonPackages;
    [ six
      # Twisted dependencies
      twisted zope_interface
      # AIO dependencies (Python 2 only).
      trollius
      futures
    ];

  doCheck = true;

  meta = {
    description = "helper library for writing code that runs unmodified on both Twisted and asyncio / Trollius.";
    homepage = http://pypi.python.org/pypi/txaio;
  };
}
