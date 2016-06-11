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

  # XXX: Use Github version
  src = fetchFromGitHub {
    owner = "jml";
    repo = "pipes-shell";
    rev = "ghc-8";
    sha256 = "148rmqvp2s085krfnww24jl9r9dfjvvg1wgy8yvwfkbjjjcxpzpk";
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

