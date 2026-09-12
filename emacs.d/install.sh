#!/bin/bash

# set -x

function fail()
{
  echo "*** ${*}"
  exit 1
}

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
[[ -d "${src}" ]] || fail "failed to detect source directory"

dst="${HOME}/.emacs.dd"
if [[ -d "${dst}" ]]; then
  echo "... '${dst}' already exists"
else
  echo "... creating '${dst}'"
  mkdir "${dst}" || fail "'mkdir ${dst}' failed"
fi

cd "${dst}" || fail "'cd ${dst}' failed"
for file in custom.el early-init.el init.el lisp; do
  if [[ -h "${dst}/${file}" ]]; then
    echo "... file '${dst}/${file}' already exists"
  else
    ln -s "${src}/${file}" "${dst}/${file}" || fail "failed to link to '${file}'"
  fi
done

# There are some brew dependencies which would be nice to have

[[ -x "$(type -p brew)" ]] || fail "'brew' not found"

brew install --no-ask coreutils
brew install --no-ask ispell
brew install --no-ask shellcheck
brew install --no-ask switchaudio-osx

# Create tap to d12frosted/emacs-plus and install pre-built binaries

brew tap d12frosted/emacs-plus
brew install --cask emacs-plus-app
