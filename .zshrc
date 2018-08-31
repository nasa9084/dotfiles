# プロンプト
autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '!'
zstyle ':vcs_info:git:*' stagedstr '+'
zstyle ':vcs_info:*' formats ' %c%u(%s:%b)'
zstyle ':vcs_info:*' actionformats ' %c%u(%s:%b|%a)'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
PROMPT=$'%B%F{green}❯❯%1(v|%1v|)%f%b %B%F{blue}%~%f%b
%{\e[38;5;240m%}%n@%m%(!.#.$) %{\e[m%}'

# ヒストリ
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
PATH="${PATH}:~/bin"

# 自動補完を有効にする
autoload -U compinit
compinit

#補完リストが多いときに尋ねない
LISTMAX=1000

# 大文字小文字区別なし補完
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

#コマンドにsudoを付けても補完
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# 入力したコマンドが存在せず、かつディレクトリ名と一致するならcd
setopt autocd

# 入力したコマンドがすでにコマンド履歴に含まれる場合古い方のコマンドを削除
setopt hist_ignore_all_dups

# スペースで始まるコマンド行はコマンド履歴に入れない
setopt hist_ignore_space

# 履歴から呼び出して実行する前に編集可能
setopt hist_verify

# 余分な空白は詰めて記録
setopt hist_reduce_blanks

# historyコマンドは履歴に保存しない
setopt hist_no_store

# 補完時に履歴を展開
setopt hist_expand

# 履歴をインクリメンタルに追加
setopt inc_append_history

# 履歴をすべてのzshプロセスで共有
setopt share_history

#ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt MARK_DIRS

# zsh簡易エディタ
autoload zed

# エディタ
export EDITOR=emacs

# 文字コード
export LANG=ja_JP.UTF-8
export KCODE=u

# alias
alias emacs=${emacsdir}\emacs
alias em="emacsclient -a '' -c"
alias emkill="emacsclient -e '(kill-emacs)'"
alias emreboot="emkill;em &"
alias emtwit="emacsclient -a '' -e '(twittering-mode)' -c &"
alias emterm="emacsclient -a '' -nw"
alias ls="ls -p"
alias la="ls -AF"
alias ll="ls -lhF"
alias lla="ls -lhAF"
alias pd="pushd"
alias ppd="popd"
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'
alias rezshrc="source ~/.zshrc"
alias setdisp="xrandr --output HDMI-1 --gamma 1.1:1:1 --brightness 0.88"

# global alias
alias -g G="| grep"
alias -g H="| head"
alias -g L="| less"
alias -g T="| tail"
alias -g W="| wc"
alias -g WL="| wc -l"

# S-[TAB]で補完を逆順
bindkey "^[[Z" reverse-menu-complete

# DELキーの修正
bindkey "^[[3~" delete-char

# config for darwin(Mac OSX)
case "${OSTYPE}" in
    darwin*)
        PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
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



# perl path
PATH="/home/nasa/perl5/bin${PATH:+:${PATH}}"; export "PATH";
PERL5LIB="/home/nasa/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/nasa/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/nasa/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/nasa/perl5"; export PERL_MM_OPT;

# go path
export GOPATH=$HOME
export PATH="$PATH:$GOPATH/bin"
# Wrap git automatically by adding the following to ~/.zshrc:

eval "$(hub alias -s)"
