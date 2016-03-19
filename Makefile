.PHONY: all clean

all: hcl/default.nix holborn-api/default.nix holborn-common-types/default.nix holborn-repo/default.nix holborn-syntax/default.nix

hcl/default.nix: hcl/hcl.cabal
	cd hcl && cabal2nix . > default.nix

holborn-api/default.nix: holborn-api/holborn-api.cabal
	cd holborn-api && cabal2nix . > default.nix

holborn-common-types/default.nix: holborn-common-types/holborn-common-types.cabal
	cd holborn-common-types && cabal2nix . > default.nix

holborn-repo/default.nix: holborn-repo/holborn-repo.cabal
	cd holborn-repo && cabal2nix . > default.nix

holborn-syntax/default.nix: holborn-syntax/holborn-syntax.cabal
	cd holborn-syntax && cabal2nix . > default.nix

clean:
	rm -f hcl/default.nix
	rm -f holborn-api/default.nix
	rm -f holborn-common-types/default.nix
	rm -f holborn-repo/default.nix
	rm -f holborn-syntax/default.nix

