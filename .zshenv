# -*- coding: utf-8 -*-

# macOS specific PATH
case "${OSTYPE}" in
    darwin*)
        # GNU make
        export PATH="/usr/local/opt/make/libexec/gnubin:${PATH}"
        export MANPATH="/usr/local/opt/make/libexec/gnuman:${MANPATH}"

        # GNU grep
        export PATH="/usr/local/opt/grep/libexec/gnubin:${PATH}"
        export MANPATH="/usr/local/opt/grep/libexec/gnuman:${MANPATH}"

        # coreutils
        export PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"
        export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:${MANPATH}"
esac

# use emacs as default editor
export EDITOR=emacs

# set language and charset
export LANG=ja_JP.UTF-8

# Golang settings
export GOPATH=${HOME}
export PATH="${PATH}:${GOPATH}/bin"


# Docker settings
# use Buildkit for docker build
export DOCKER_BUILDKIT=1

# load secrets
source ${HOME}/.zshenv.secret
