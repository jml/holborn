.PHONY: all check clean

all: hcl/default.nix holborn-api/default.nix holborn-common-types/default.nix holborn-repo/default.nix holborn-prelude/default.nix holborn-syntax/default.nix

check:
	nix-build --no-out-link ./integration-tests

clean:
	rm -f hcl/default.nix
	rm -f hcl/shell.drv
	rm -f holborn-api/default.nix
	rm -f holborn-api/shell.drv
	rm -f holborn-common-types/default.nix
	rm -f holborn-common-types/shell.drv
	rm -f holborn-prelude/default.nix
	rm -f holborn-prelude/shell.drv
	rm -f holborn-repo/default.nix
	rm -f holborn-repo/shell.drv
	rm -f holborn-syntax/default.nix
	rm -f holborn-syntax/shell.drv

hcl/default.nix: hcl/hcl.cabal
	cd hcl && cabal2nix . > default.nix

hcl/shell.drv: hcl/shell.nix hcl/default.nix
	nix-instantiate ./hcl/shell.nix --indirect --add-root $(shell pwd)/hcl/shell.drv > /dev/null

holborn-api/default.nix: holborn-api/holborn-api.cabal
	cd holborn-api && cabal2nix . > default.nix

holborn-api/shell.drv: holborn-api/shell.nix holborn-api/default.nix
	nix-instantiate ./holborn-api/shell.nix --indirect --add-root $(shell pwd)/holborn-api/shell.drv > /dev/null

holborn-common-types/default.nix: holborn-common-types/holborn-common-types.cabal
	cd holborn-common-types && cabal2nix . > default.nix

holborn-common-types/shell.drv: holborn-common-types/shell.nix holborn-common-types/default.nix
	nix-instantiate ./holborn-common-types/shell.nix --indirect --add-root $(shell pwd)/holborn-common-types/shell.drv > /dev/null

holborn-prelude/default.nix: holborn-prelude/holborn-prelude.cabal
	cd holborn-prelude && cabal2nix . > default.nix

holborn-prelude/shell.drv: holborn-prelude/shell.nix holborn-prelude/default.nix
	nix-instantiate ./holborn-prelude/shell.nix --indirect --add-root $(shell pwd)/holborn-prelude/shell.drv > /dev/null

holborn-repo/default.nix: holborn-repo/holborn-repo.cabal
	cd holborn-repo && cabal2nix . > default.nix

holborn-repo/shell.drv: holborn-repo/shell.nix holborn-repo/default.nix
	nix-instantiate ./holborn-repo/shell.nix --indirect --add-root $(shell pwd)/holborn-repo/shell.drv > /dev/null

holborn-syntax/default.nix: holborn-syntax/holborn-syntax.cabal
	cd holborn-syntax && cabal2nix . > default.nix

holborn-syntax/shell.drv: holborn-syntax/shell.nix holborn-syntax/default.nix
	nix-instantiate ./holborn-syntax/shell.nix --indirect --add-root $(shell pwd)/holborn-syntax/shell.drv > /dev/null


