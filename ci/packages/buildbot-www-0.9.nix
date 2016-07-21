{ stdenv, buildPythonPackage, fetchurl, pythonPackages
, buildbot, buildbot-pkg
}:
buildPythonPackage (rec {
  name = "buildbot-www-${version}";
  version = "0.9.0b9";
  format = "wheel";

  src = fetchurl {
    url = "https://pypi.python.org/packages/2f/91/63e76ad6c95ede1993a90464d8123504d217755757e86b6872cb161de955/buildbot_www-0.9.0b9-py2-none-any.whl";
    sha256 = "1n65k2wrvdy840cpgm21fx0m8wdq7w926vaaizyb9s3yyyimyci0";
  };

  buildInputs = [ buildbot buildbot-pkg pythonPackages.mock ];

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
