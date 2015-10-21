with (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz) {}).pkgs;
pkgs.buildPythonPackage {
  name = "holborn-people";
  srcs = ./.;
  propagatedBuildInputs = [
    pythonPackages.hypothesis
    pythonPackages.django
    pythonPackages.sqlite3
  ];
}
