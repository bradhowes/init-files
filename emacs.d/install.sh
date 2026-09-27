#!/bin/bash

# set -x

function fail()
{
  echo "*** ${*}"
  exit 1
}

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
[[ -d "${src}" ]] || fail "failed to detect source directory"

dst="${HOME}/.emacs.d"
if [[ -d "${dst}" ]]; then
  echo "... '${dst}' already exists"
else
  echo "... creating '${dst}'"
  mkdir "${dst}" || fail "'mkdir ${dst}' failed"
fi

cd "${dst}" || fail "'cd ${dst}' failed"
for file in custom.el early-init.el init.el user-lisp; do
  if [[ -h "${dst}/${file}" ]]; then
    echo "... file '${dst}/${file}' already exists"
  else
    ln -s "${src}/${file}" "${dst}/${file}" || fail "failed to link to '${file}'"
  fi
done

[[ -f "${dst}/projects" ]] || cp "${src}/projects" "${dst}/projects"

# Install homebrew base
[[ -d "/opt/homebrew" ]] || curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh

# Install dependencies
for what in aspell coreutils grep ispell rg shellcheck; do
  echo "-- installing ${what}"
  brew install ${what}
done

# Install Emacs
if [[ ! -d /Applications/Emacs.app ]]; then
  rm -rf "/Applications/Emacs Client.app"
  brew tap d12frosted/emacs-plus
  brew trust d12frosted/emacs-plus
  brew install --cask emacs-plus-app
fi
