{ stdenv, buildPythonPackage, fetchurl, pythonPackages,
  callPackage
}:

let
  buildbotPkg = callPackage ./buildbot-pkg-0.9.nix {};
in

buildPythonPackage (rec {
  name = "buildbot-waterfall-view-${version}";
  version = "0.9.0b9";

  src = fetchurl {
    url = "https://pypi.python.org/packages/bb/b1/ec561ebcb6894a84d71b6c821fa26cf8aa9819078d0fdc406c2f34f5ab51/${name}.tar.gz";
    sha256 = "0xcl1msf3s04wj9g12db45a3f3br5xybsvfdjan8f5klp6m5kw40";
  };

  buildInputs = [ buildbotPkg ];

  doCheck = true;

  postInstall = ''
  '';

  meta = with stdenv.lib; {
    homepage = http://buildbot.net/;
    license = stdenv.lib.licenses.gpl2Plus;
    # Of course, we don't really need that on NixOS.  :-)
    description = "";
    longDescription =
      '' The BuildBot is a system to automate the compile/test cycle
         required by most software projects to validate code changes.  By
         automatically rebuilding and testing the tree each time something
         has changed, build problems are pinpointed quickly, before other
         developers are inconvenienced by the failure.  The guilty
         developer can be identified and harassed without human
         intervention.  By running the builds on a variety of platforms,
         developers who do not have the facilities to test their changes
         everywhere before checkin will at least know shortly afterwards
         whether they have broken the build or not.  Warning counts, lint
         checks, image size, compile time, and other build parameters can
         be tracked over time, are more visible, and are therefore easier
         to improve.

         The overall goal is to reduce tree breakage and provide a platform
         to run tests or code-quality checks that are too annoying or
         pedantic for any human to waste their time with.  Developers get
         immediate (and potentially public) feedback about their changes,
         encouraging them to be more careful about testing before checking
         in code.
      '';
    maintainers = with maintainers; [ bjornfor ];
    platforms = platforms.all;
  };
})
