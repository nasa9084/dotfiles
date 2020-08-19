# -*- coding: utf-8 -*-

# use emacs as default editor
export EDITOR=emacs

# set language and charset
export LANG=ja_JP.UTF-8

# Golang settings
export GOPATH=${HOME}
export PATH="${PATH}:${GOPATH}/bin"

# load secrets

source ${HOME}/.zshenv.secret
