{ stdenv, buildPythonPackage, fetchurl, pythonPackages
}:

buildPythonPackage (rec {
  name = "buildbot-worker-${version}";
  version = "0.9.0b9";

  src = fetchurl {
    url = "https://pypi.python.org/packages/45/d6/12d115c08959af2e8c99f7b2543f458c277f7ad0a6f8bf9415d232381927/${name}.tar.gz";
    sha256 = "06zbiqyzynwiprlc17n14czb97d2f7zbja4i3sq6g3lf80vxg7xa";
  };

  propagatedBuildInputs =
    with pythonPackages;
    [ twisted future ];

  # What's up with this?! 'trial' should be 'test', no?
  #
  # running tests
  # usage: setup.py [global_opts] cmd1 [cmd1_opts] [cmd2 [cmd2_opts] ...]
  #    or: setup.py --help [cmd1 cmd2 ...]
  #    or: setup.py --help-commands
  #    or: setup.py cmd --help
  #
  # error: invalid command 'trial'
  doCheck = false;

  meta = with stdenv.lib; {
    homepage = http://buildbot.net/;
    license = stdenv.lib.licenses.gpl2Plus;
    maintainers = with maintainers; [ bjornfor ];
    platforms = platforms.all;
  };
})
