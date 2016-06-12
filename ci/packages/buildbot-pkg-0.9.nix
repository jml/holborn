{ stdenv, buildPythonPackage, fetchurl, pythonPackages
}:

buildPythonPackage (rec {
  name = "buildbot-pkg-${version}";
  version = "0.9.0b9";

  src = fetchurl {
    url = "https://pypi.python.org/packages/f2/37/c554c8a75273fbf7ad2e7b5dcc782f942b770b27f690d5eab42f8f758b84/${name}.tar.gz";
    sha256 = "16lpji46hyn1d2lnkzm0nsf1lxx067p3rc0249m63lq2nnkyapcc";
  };

  doCheck = true;

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
