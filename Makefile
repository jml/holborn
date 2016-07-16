.PHONY: all check clean

HASKELL_PROJECTS = hcl holborn-api holborn-common-types holborn-repo holborn-prelude holborn-syntax

cabal_files = $(foreach proj,$(HASKELL_PROJECTS),$(proj)/$(proj).cabal)
nix_exprs = $(foreach proj,$(HASKELL_PROJECTS),$(proj)/default.nix)

define generate_default_nix
    $1/default.nix: $1/$1.cabal
		cd $1 && cabal2nix . > default.nix
endef

all: $(nix_exprs)

check:
	nix-build --no-out-link ./integration-tests
	nix-shell -p '(import ./integration-tests).ssh-authz' --command "holborn-ssh-authz-test"

clean:
	rm -f $(nix_exprs)
	rm -f $(cabal_files)


holborn-common-types/holborn-common-types.cabal: holborn-common-types/package.yaml
	hpack --silent holborn-common-types/

holborn-prelude/holborn-prelude.cabal: holborn-prelude/package.yaml
	hpack --silent holborn-prelude/

hcl/hcl.cabal: hcl/package.yaml
	hpack --silent hcl/


$(foreach proj,$(HASKELL_PROJECTS),$(eval $(call generate_default_nix,$(proj))))
