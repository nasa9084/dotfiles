# -*- coding: utf-8 -*-

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
