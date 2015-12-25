# プロンプト
PROMPT=$'%{\e[38;5;240m%}%n@%m%(!.#.$) %{\e[m%}'
RPROMPT=$'[%~]'

# ヒストリ
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
PATH=${PATH}:~/bin

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

# 入力したコマンドが存在しない場合bash同様に提案
function command_not_found_handler {
	/usr/bin/python /usr/lib/command-not-found -- $1
	return $?
}

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
alias la="ls -AF"
alias ll="ls -lhF"
alias lla="ls -lhAF"
alias rm="rm -i"
alias mv="mv -i"
alias pd="pushd"
alias ppd="popd"
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'
alias rezshrc="source ~/.zshrc"
alias setdisp="xrandr --output HDMI-0 --gamma 1.1:1:1 --brightness 0.88"

# global alias
alias -g G="| grep"
alias -g H="| head"
alias -g L="| less"
alias -g T="| tail"
alias -g W="| wc"
alias -g WL="| wc -l"

# suffix alias
alias -s py="python"
alias -s txt="less"
alias -s rb="ruby"

# S-[TAB]で補完を逆順
bindkey "^[[Z" reverse-menu-complete

# DELキーの修正
bindkey "^[[3~" delete-char

# anyenv
export PATH="$HOME/.anyenv/bin:$PATH"
eval "$(anyenv init - zsh)"

# added by Anaconda 2.3.0 installer
export PATH="/home/nasa/anaconda/bin:$PATH"
