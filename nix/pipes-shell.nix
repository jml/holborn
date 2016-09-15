# pipes-shell 0.1.4 does not build on ghc 8.0.
#
# This uses a custom fork to fix it until we can fix upstream or switch to an
# alternative like process-streaming or pipes-cliff.

{ mkDerivation, async, base, bytestring, directory, hspec, pipes
, pipes-bytestring, pipes-safe, process, stm, stm-chans, text
, stdenv
, fetchFromGitHub
}:
mkDerivation {
  pname = "pipes-shell";
  version = "0.1.4";

  src = fetchFromGitHub {
    owner = "jml";
    repo = "pipes-shell";
    rev = "ghc-8";
    sha256 = "1w2w5ccbfq4l1vp4drh52sv3x5168qmp5k4rb01j1s9n6slzdqr3";
  };

  libraryHaskellDepends = [
    async base bytestring pipes pipes-bytestring pipes-safe process stm
    stm-chans text
  ];
  testHaskellDepends = [
    async base bytestring directory hspec pipes pipes-bytestring
    pipes-safe process stm stm-chans text
  ];
  jailbreak = true;
  description = "Create proper Pipes from System.Process";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = [ "x86_64-darwin" ];
}
