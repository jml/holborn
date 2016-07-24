.PHONY: all check clean

HASKELL_PROJECTS = hcl holborn-api holborn-common-types holborn-repo holborn-prelude holborn-ssh holborn-syntax holborn-proxy

integration_test_output_dir = .makefile-build
cabal_files = $(foreach proj,$(HASKELL_PROJECTS),$(proj)/$(proj).cabal)
nix_exprs = $(foreach proj,$(HASKELL_PROJECTS),$(proj)/default.nix)

define generate_default_nix
    $1/default.nix: $1/$1.cabal
		cd $1 && cabal2nix . > default.nix
endef

all: $(nix_exprs)

# Do a little bit of a dance to make sure that we leave nix-build symlinks
# around, but in a place where they won't show up in git or get in the way.
#
# We do this so that there's a GC root, so that when we run nix-collect-gc we
# don't accidentally blow away a pile of stuff like ghc that's been around for
# ages.
check: $(integration_test_output_dir)
	rm -f $(integration_test_output_dir)/*
	nix-build -o $(integration_test_output_dir)/result ./integration-tests
	nix-shell -p '(import ./integration-tests).ssh-authz' --command "holborn-ssh-authz-test"

$(integration_test_output_dir):
	mkdir -p $(integration_test_output_dir)

clean:
	rm -f $(nix_exprs)
	rm -f $(cabal_files)
	rm -rf $(integration_test_output_dir)

holborn-api/holborn-api.cabal: holborn-api/package.yaml
	hpack --silent holborn-api/

holborn-proxy/holborn-proxy.cabal: holborn-proxy/package.yaml
	hpack --silent holborn-proxy/

holborn-common-types/holborn-common-types.cabal: holborn-common-types/package.yaml
	hpack --silent holborn-common-types/

holborn-prelude/holborn-prelude.cabal: holborn-prelude/package.yaml
	hpack --silent holborn-prelude/

holborn-repo/holborn-repo.cabal: holborn-repo/package.yaml
	hpack --silent holborn-repo/

holborn-ssh/holborn-ssh.cabal: holborn-ssh/package.yaml
	hpack --silent holborn-ssh/

holborn-syntax/holborn-syntax.cabal: holborn-syntax/package.yaml
	hpack --silent holborn-syntax/

hcl/hcl.cabal: hcl/package.yaml
	hpack --silent hcl/


$(foreach proj,$(HASKELL_PROJECTS),$(eval $(call generate_default_nix,$(proj))))
