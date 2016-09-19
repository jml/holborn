{ pythonPackages, lib }:
with pythonPackages; buildPythonPackage rec {
  name = "holborn-models";
  src = lib.sourceFilesBySuffices ../models [".py"];
  doCheck = false;
  preBuild = ''
    rm -rf build
  '';
  propagatedBuildInputs = [ django psycopg2 ];
}
