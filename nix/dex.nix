{ buildGoPackage, git, fetchFromGitHub }:
buildGoPackage rec {
  version    = "v0.5.1";
  name = "dex-${version}";
  goPackagePath = "github.com/coreos/dex";

  # dex uses a custom builder and doesn't follow the new go (1.5+)
  # conventions so we can't use the nix builder / installer:
  buildPhase = ''
    cd go/src/github.com/coreos/dex
    substituteInPlace env --replace "VERSION=\$(./git-version)" "VERSION=${version}"
    bash build
  '';
  installPhase = ''
    mkdir $out # needed but empty ...
    mkdir $bin
    cp -r bin $bin/
    cp -r static $bin/
  '';

  enableParallelBuilding = false;
  src = fetchFromGitHub {
      rev    = version;
      owner  = "coreos";
      repo   = "dex";
      sha256 = "05dzzzh88hbcymhccxzza041wvhr2k167nsqc95cw6vfpyllmshv";
  };
}
