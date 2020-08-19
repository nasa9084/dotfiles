# -*- coding: utf-8 -*-

# command history
HISTFILE=$HOME/.zsh_history
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

# autocd
setopt autocd

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

# zsh editor
autoload zed

# alias
alias emacs=${emacsdir}\emacs
alias em="emacsclient -a '' -c"
alias emkill="emacsclient -e '(kill-emacs)'"
alias emreboot="emkill;em &"
alias emtwit="emacsclient -a '' -e '(twittering-mode)' -c &"
alias emterm="emacsclient -a '' -nw"
alias ls="exa -Fgh --git --time-style=long-iso"
alias la="ls -a"
alias ll="ls -l"
alias lla="ls -la"
alias pd="pushd"
alias ppd="popd"
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'
alias rezshrc="source ~/.zshrc"
alias setdisp="xrandr --output HDMI-1 --gamma 1.1:1:1 --brightness 0.88"
alias cat="bat"
alias grep="grep --binary-files=without-match --exclude-dir=.git"

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

# config for darwin(Mac OSX)
case "${OSTYPE}" in
    darwin*)
        export PATH=/usr/local/opt/coreutils/libexec/gnubin:${PATH}
        export MANPATH=/usr/local/opt/coreutils/libexec/gnuman:${MANPATH}
esac

stty stop undef
stty start undef

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

# use Buildkit for docker build
export DOCKER_BUILDKIT=1

# Wrap git automatically by adding the following to ~/.zshrc:
eval "$(hub alias -s)"

# load secrets
source ${HOME}/.zshrc.secret

# starship
eval "$(starship init zsh)"
