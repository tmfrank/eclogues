#!/usr/bin/env bash

set -e

BIN_DIR="$(stack path --local-install-root)/bin"
REL_DIR="./release"

rm -r $REL_DIR
mkdir -p $REL_DIR/tmp
stack clean
stack build --ghc-options -O2

cd $REL_DIR

for bin in "eclogues-api" "eclogues-subexecutor" "eclogues-client" "eclogues-mock"; do
    cp $BIN_DIR/$bin tmp/$bin
    strip -p --strip-unneeded --remove-section=.comment -otmp/$bin.stripped tmp/$bin
    upx --no-env -8 -o$bin tmp/$bin.stripped
done

tar --create --xz --no-xattrs --no-acls --owner=0 --group=0 --numeric-owner --mode=u=rwx,g=rx,o=rx eclogues-client eclogues-subexecutor > eclogues-slave.tar.xz
