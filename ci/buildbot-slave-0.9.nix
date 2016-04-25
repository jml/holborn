{ stdenv, buildPythonPackage, fetchurl, pythonPackages, callPackage
}:

buildPythonPackage (rec {
  name = "buildbot-slave-${version}";
  version = "0.9.0b7";

  src = fetchurl {
    url = "https://pypi.python.org/packages/d9/57/37e529fe46bc176aa78ad7a589d0c7aca2bb104c556d834d44a22df99f23/${name}.tar.gz";
    sha256 = "0vi64p328d4748806l7m4rx257szsg59vbydk5j9bf2aqcjan033";
  };

  propagatedBuildInputs =
    with pythonPackages;
    [ twisted future ];

  doCheck = false;

  meta = with stdenv.lib; {
    homepage = http://buildbot.net/;
    license = stdenv.lib.licenses.gpl2Plus;
    maintainers = with maintainers; [ bjornfor ];
    platforms = platforms.all;
  };
})
