#!/bin/zsh
# -*- coding: utf-8 -*-



# macOS specific PATH
case "${OSTYPE}" in
    darwin*)
        case "$(uname -m)" in
            amd64)
                # disable to call path_helper after this file but call first
                setopt no_global_rcs
                eval $(/usr/libexec/path_helper -s)

                # some brew formulae installs executables into sbin
                export PATH="/usr/local/sbin:$PATH"

                # GNU make
                export PATH="/usr/local/opt/make/libexec/gnubin:${PATH}"
                export MANPATH="/usr/local/opt/make/libexec/gnuman:${MANPATH}"

                # GNU grep
                export PATH="/usr/local/opt/grep/libexec/gnubin:${PATH}"
                export MANPATH="/usr/local/opt/grep/libexec/gnuman:${MANPATH}"

                # GNU sed
                export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:${PATH}"
                export MANPATH="/usr/local/opt/gnu-sed/libexec/gnuman:${MANPATH}"

                # GNU tar
                export PATH="/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"
                export MANPATH="/usr/local/opt/gnu-tar/libexec/gnuman:${MANPATH}"

                # coreutils
                export PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"
                export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:${MANPATH}"
                ;;
            arm64)
                # disable to call path_helper after this file but call first
                setopt no_global_rcs
                eval $(/usr/libexec/path_helper -s)

                # some brew formulae installs executables into sbin
                export PATH="/usr/local/sbin:${PATH}"

                # GNU make
                export PATH="/opt/homebrew/opt/make/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/opt/make/libexec/gnuman:${MANPATH}"

                # GNU grep
                export PATH="/opt/homebrew/opt/grep/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/opt/grep/libexec/gnuman:${MANPATH}"

                # GNU sed
                export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/opt/gnu-sed/libexec/gnuman:${MANPATH}"

                # GNU tar
                export PATH="/opt/homebrew/opt/gnu-tar/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/opt/gnu-tar/libexec/gnuman:${MANPATH}"

                # coreutils
                export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/opt/coreutils/libexec/gnuman:${MANPATH}"

                # findutils; xargs or find
                export PATH="/opt/homebrew/opt/findutils/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/opt/findutils/libexec/gnuman:${PATH}"

                # mysql-client
                export PATH="/opt/homebrew/opt/mysql-client/bin:${PATH}"

                # Rancher Desktop
                export PATH="${HOME}/.rd/bin:${PATH}"

                # Docker platform
                export DOCKER_DEFAULT_PLATFORM=linux/amd64
        esac
esac


export ASDF_DATA_DIR=${HOME}/.asdf
export PATH="${ASDF_DATA_DIR}/shims:${PATH}"

# use emacs as default editor
export EDITOR=emacs

# set language and charset
export LANG=ja_JP.UTF-8

# Golang settings
export GOPATH=${HOME}
export PATH="${PATH}:${GOPATH}/bin"

# PATH to extension scripts
export PATH="${PATH}:${HOME}/.extensions"

# Docker settings
# use Buildkit for docker build
export DOCKER_BUILDKIT=1

# krew - plugin system for kubectl
export PATH="${PATH}:${HOME}/.krew/bin"

# pipx
export PATH="${PATH}:${HOME}/.local/bin"

# load secrets
source "${HOME}/.zshenv.secret"
