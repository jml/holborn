with (import <nixpkgs> {}).pkgs;

pkgs.buildPythonPackage {
  name = "holborn-people";
  srcs = ./.;
  propagatedBuildInputs = [
    pythonPackages.hypothesis
    pythonPackages.django
    pythonPackages.sqlite3
  ];
}
