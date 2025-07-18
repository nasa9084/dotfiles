#!/bin/zsh
# -*- coding: utf-8 -*-
# shellcheck disable=all

# disable START/STOP flow control
stty -ixon

# command history
HISTFILE=${HOME}/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# auto completion
autoload -U compinit
compinit

# not ask too many completion
LISTMAX=1000

# case insentive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# completion with sudo
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# ignore duplicate in history
setopt hist_ignore_all_dups

# ignore begin with space
setopt hist_ignore_space

# edit before run when use history
setopt hist_verify

# reduce blanks in history
setopt hist_reduce_blanks

# not store `history` command in history
setopt hist_no_store

# completion with history
setopt hist_expand

# increment append to history
setopt inc_append_history

# share history all zsh processes
setopt share_history

# if complete dir name, add trailing slash
setopt MARK_DIRS

# alias
alias ls="eza --classify=auto -gh --git --time-style=long-iso"
alias la="ls -a"
alias ll="ls -l"
alias lla="ls -la"
alias pd="pushd"
alias ppd="popd"
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'
alias rezshrc="source ~/.zshrc"
alias cat="bat"
alias grep="rg"
alias jq="~/bin/jq" # https://gist.github.com/nasa9084/caee63465acb712d43301c488534e7b5

if [[ "${OSTYPE}" == "darwin"* ]] && [ "$(uname -m)" = "arm64" ]
then
    alias python="python3"
fi

# global alias
alias -g G="| grep"
alias -g H="| head"
alias -g L="| less"
alias -g T="| tail"
alias -g W="| wc"
alias -g WL="| wc -l"

# reverse completion with S-<TAB>
bindkey "^[[Z" reverse-menu-complete

# now we can use del
bindkey "^[[3~" delete-char

# peco
function peco-src () {
  local selected_dir=$(ghq list -p | perl -nlpe 's[.*src/(.*)][$1\0$_]' | peco --null)
  if [ -n "$selected_dir" ]; then
    BUFFER="cd ${selected_dir}"
    zle accept-line
  fi
  zle clear-screen
}

zle -N peco-src
bindkey '^S' peco-src

function peco-history-selection() {
    BUFFER="$(history -nr 1 | awk '!a[$0]++' | peco --query "$LBUFFER" | sed 's/\\n/\n/g')"
    zle clear-screen
}

zle -N peco-history-selection
bindkey '^R' peco-history-selection

# Wrap git automatically by adding the following to ~/.zshrc:
eval "$(hub alias -s)"

# load secrets
source "${HOME}/.zshrc.secret"

# starship
eval "$(starship init zsh)"

function preexec_kubectx_asdf_kubectl() {
    IFS=" " read -rA cmd <<< "${1}"

    if [ "${cmd[1]}" != "kubectx" ] || [ "${#cmd}" -ne 2 ]
    then
        return
    fi

    if ! kubectl version 2>&1 > /dev/null
    then
        echo "cannot get Kubernetes server version."
        return
    fi

    version=$(kubectl --context="${cmd[2]}" version -o json | jq -r '.serverVersion.gitVersion' | tr -d 'v' 2>/dev/null)

    echo "Switch kubectl version to ${version}"
    if ! asdf list kubectl | grep "${version}" > /dev/null
    then
        echo "kubectl ${version} has not been installed."
        asdf install kubectl "${version}"
    fi
    asdf set -u kubectl "${version}"
}

autoload -Uz add-zsh-hook
add-zsh-hook preexec preexec_kubectx_asdf_kubectl

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
