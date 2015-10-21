with (import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz) {}).pkgs;
let
# XXX: We probably don't need this any more
  hypothesis = pythonPackages.buildPythonPackage rec {
    name = "hypothesis-1.10.6";
    doCheck = false;
    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/source/h/hypothesis/${name}.tar.gz";
      md5 = "333f7bd8b4b0311ea6779cb601de254e";
    };
    meta = {
      description = "A Python library for property based testing";
      homepage = https://github.com/DRMacIver/hypothesis;
      license = licenses.mpl20;
    };
  };
in
pkgs.buildPythonPackage {
  name = "holborn-people";
  srcs = ./.;
  propagatedBuildInputs = [
    hypothesis
    pythonPackages.django
    pythonPackages.sqlite3
  ];
}
