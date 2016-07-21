{ buildPythonPackage }:
buildPythonPackage {
  name = "holborn_bb-1.0.0";

  src = ./.;

  meta = {
    description = "Buildbot configuration for Holborn";
  };
}
