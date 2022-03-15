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
                export PATH="/usr/local/sbin:$PATH"

                # GNU make
                export PATH="/opt/homebrew/Cellar/make/4.3/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/Cellar/make/4.3/libexec/gnuman:${MANPATH}"

                # GNU grep
                export PATH="/opt/homebrew/Cellar/grep/3.7/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/Cellar/grep/3.7/libexec/gnuman:${MANPATH}"

                # GNU sed
                export PATH="/opt/homebrew/Cellar/gnu-sed/4.8/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/Cellar/gnu-sed/4.8/libexec/gnuman:${MANPATH}"

                # GNU tar
                export PATH="/opt/homebrew/Cellar/gnu-tar/1.34/libexec/gnubin:$PATH"
                export MANPATH="/opt/homebrew/Cellar/gnu-tar/1.34/libexec/gnuman:${MANPATH}"

                # coreutils
                export PATH="/opt/homebrew/Cellar/coreutils/9.0_1/libexec/gnubin:${PATH}"
                export MANPATH="/opt/homebrew/Cellar/coreutils/9.0_1/libexec/gnuman:${MANPATH}"
        esac
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

# krew - plugin system for kubectl
export PATH="${PATH}:${HOME}/.krew/bin"

# load secrets
source ${HOME}/.zshenv.secret
