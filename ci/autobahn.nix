{ stdenv, buildPythonPackage, fetchurl, callPackage
, pythonPackages }:

# XXX: Assumes Python 2.7

buildPythonPackage rec {
  name = "autobahn-${version}";
  version = "0.13.1";

  src = fetchurl {
    url = "https://pypi.python.org/packages/source/a/autobahn/${name}.tar.gz";
    sha256 = "0ja92091ll8rn8n7mgz29xa1y3l0xm8zvj42shwkxazwpp545xz1";
  };

  propagatedBuildInputs =
    with pythonPackages;
    [ six
      # TODO: Use txaio from pythonPackages once it's there.
      (callPackage ./txaio.nix {})
    ];

  # TODO: Maybe this works, haven't tested.
  doCheck = false;

  meta = {
    description = "WebSocket client & server library, WAMP real-time framework";
    homepage = http://pypi.python.org/pypi/autobahn;
    #license = licenses.mit;
  };
}
