#!/usr/bin/env bash
# Build an ELPA tarball installable with `package-install-file': dist/opencode-VERSION.tar
set -euo pipefail

version=${1:-$(sed -n 's/^;; Package-Version: //p' opencode.el)}
requires=$(sed -n 's/^;; Package-Requires: //p' opencode.el)
test -n "$version"
test -n "$requires"

stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT
dest="$stage/opencode-$version"
mkdir "$dest"
cp *.el "$dest/"
rm -f "$dest"/*-test.el
printf '(define-package "opencode" "%s" "Emacs interface to opencode." '"'"'%s)\n' \
	"$version" "$requires" >"$dest/opencode-pkg.el"

mkdir -p dist
rm -f dist/opencode-*.tar
tar cf "dist/opencode-$version.tar" -C "$stage" "opencode-$version"
