.PHONY: all check clean

HASKELL_PROJECTS = hcl holborn-api holborn-common-types holborn-repo holborn-prelude holborn-syntax

cabal_files = $(foreach proj,$(HASKELL_PROJECTS),$(proj)/$(proj).cabal)
nix_exprs = $(foreach proj,$(HASKELL_PROJECTS),$(proj)/default.nix)
shell_drvs = $(foreach proj,$(HASKELL_PROJECTS),$(proj)/shell.drv)

define generate_default_nix
    $1/default.nix: $1/$1.cabal
		cd $1 && cabal2nix . > default.nix
endef

all: $(nix_exprs) $(shell_drvs)

check:
	nix-build --no-out-link ./integration-tests

clean:
	rm -f $(nix_exprs) $(shell_drvs)

$(foreach proj,$(HASKELL_PROJECTS),$(eval $(call generate_default_nix,$(proj))))

$(shell_drvs): %/shell.drv: %/shell.nix %/default.nix
	nix-instantiate ./$*/shell.nix --indirect --add-root $(shell pwd)/$*/shell.drv > /dev/null
