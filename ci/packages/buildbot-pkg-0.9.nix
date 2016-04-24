{ stdenv, buildPythonPackage, fetchurl, pythonPackages
}:

buildPythonPackage (rec {
  name = "buildbot-pkg-${version}";
  version = "0.9.0b7";

  src = fetchurl {
    url = "https://pypi.python.org/packages/3a/c1/adc89f894d1b060ac81d901a00231fd9ef96fff0d813d6b23d570216c5be/${name}.tar.gz";
    sha256 = "02gg9mk0v95dwbz330yilmlg94gz9vqr6jk3bgjxdxpm27zmqp2b";
  };

  doCheck = false;

  meta = with stdenv.lib; {
    homepage = http://buildbot.net/;
    license = stdenv.lib.licenses.gpl2Plus;
    # Of course, we don't really need that on NixOS.  :-)
    description = "Buildbot packaging tools";
    longDescription =
      '' This package contains utilities and common code for building and testing www
         plugins.
      '';
    maintainers = with maintainers; [ bjornfor ];
    platforms = platforms.all;
  };
})
