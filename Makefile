# -*- Makefile -*- This minimal Makefile delegates most work to the asdf-tools script
#
# End-Users, all you need to do is:
#   make
#
# Vendors, you may want to test your implementation with:
#   make test l=sbcl
#
# Other targets are for maintainer use only.
#

l ?= sbcl

# Default action: bootstrap asdf.lisp
# That's the only thing that we really need before we may invoke asdf-builder.
all: build/asdf.lisp
	@: # This dummy action is necessary so the all target does not invoke the fallback action.

header_lisp := header.lisp
driver_lisp := uiop/package.lisp uiop/common-lisp.lisp uiop/utility.lisp uiop/os.lisp uiop/pathname.lisp uiop/filesystem.lisp uiop/stream.lisp uiop/image.lisp uiop/run-program.lisp uiop/lisp-build.lisp uiop/configuration.lisp uiop/backward-driver.lisp uiop/driver.lisp
defsystem_lisp := upgrade.lisp component.lisp system.lisp cache.lisp find-system.lisp find-component.lisp operation.lisp action.lisp lisp-action.lisp plan.lisp operate.lisp output-translations.lisp source-registry.lisp backward-internals.lisp parse-defsystem.lisp bundle.lisp concatenate-source.lisp backward-interface.lisp package-inferred-system.lisp interface.lisp user.lisp footer.lisp

# Making ASDF itself should be our first, default, target:
build/asdf.lisp: $(header_lisp) $(driver_lisp) $(defsystem_lisp)
	mkdir -p build
	rm -f $@
	cat $^ > $@

# These targets are used during tests to ensure the Makefile is in synch with the .asd files.
driver-files:
	@echo $(driver_lisp)

defsystem-files:
	@echo $(defsystem_lisp)

### exclude source files from fallback rule.
%.lisp:
	@:
Makefile:
	@:

### Default fall back rule: delegate to asdf-tools.
# Note that the l= L= etc. are the only way I (Faré) have found to
# pass arguments from the Makefile to the underlying script:
# l= overrides $ASDF_TEST_LISPS to specify which lisp implementations to use
# L= overrides $ASDF_UPGRADE_TEST_LISPS (defaults to the former) to lisps during upgrade
# s= overrides $ASDF_TEST_SYSTEMS to specify systems with which to test ASDF
# t= overrides $ASDF_TESTS to specify test script patterns to use (default to *.script)
# u= overrides $ASDF_UPGRADE_TEST_TAGS to specify versions to upgrade from (e.g. 3.0.3 or REQUIRE)
# U= overrides $ASDF_UPGRADE_TEST_METHODS to specify upgrade methods
# v= overrides the default next version for bump-version or bump.
# see in tools/test-environment.lisp for details.
# Note that because of how make and the shell quote arguments,
# thou shalt not use the single-quote character in any of the short x= parameters
# (but you may use them in the long variant in the exported environment variable).
# To have a list of commands, see make help or ./tools/asdf-tools help
# Note that when you call ./tools/asdf-tools directly,
# you may have to use positional parameters instead (unless you use env as below), as in
#   ./tools/asdf-tools bump 3.2.1
# instead of
#   make bump v=3.2.1
# or
#   ./tools/asdf-tools env v=3.2.1 bump
%: build/asdf.lisp
	./tools/asdf-tools env l='$l' L='$L' u='$u' U='$u' v='$v' s='$s' t='$t' $@

# This is the list of phony targets from this file
.PHONY: all driver-files defsystem-files

# The text below was automaticaly generated by `make --silent makefile-targets`
# then manually inserted here to provide for completion:
archive:
build-asdf:
bump:
bump-version:
check-all-results:
check-all-scripts-results:
check-all-upgrade-results:
clean:
debian-package:
doc:
extract:
extract-all-tagged-asdf:
extract-tagged-asdf:
fix-local-git-tags:
fix-remote-git-tags:
git-all-committed-p:
help:
install:
install-asdf:
link-archive:
list-source-registry:
load:
make-archive:
makefile-targets:
merge-master-into-release:
publish-archive:
publish-debian-package:
push:
re:
show-commands:
t:
test:
test-all:
test-all-clean-load:
test-all-no-stop:
test-all-no-upgrade:
test-all-no-upgrade-no-stop:
test-all-scripts:
test-all-scripts-no-stop:
test-all-upgrade:
test-all-upgrade-no-stop:
test-basic:
test-clean-load:
test-load-systems:
test-scripts:
test-upgrade:
u:
wc:
website:
