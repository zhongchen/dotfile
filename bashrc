# -----------------------------------
# -- 1.1) Set up umask permissions --
# -----------------------------------
#  The following incantation allows easy group modification of files.
#  See here: http://en.wikipedia.org/wiki/Umask
#
#     umask 002 allows only you to write (but the group to read) any new
#     files that you create.
#
#     umask 022 allows both you and the group to write to any new files
#     which you make.
#
#  In general we want umask 022 on the server and umask 002 on local
#  machines.
#
#  The command 'id' gives the info we need to distinguish these cases.
#
#     $ id -gn  #gives group name
#     $ id -un  #gives user name
#     $ id -u   #gives user ID
#
#  So: if the group name is the same as the username OR the user id is not
#  greater than 99 (i.e. not root or a privileged user), then we are on a
#  local machine (check for yourself), so we set umask 002.
#
#  Conversely, if the default group name is *different* from the username
#  AND the user id is greater than 99, we're on the server, and set umask
#  022 for easy collaborative editing.
if [ "`id -gn`" == "`id -un`" -a `id -u` -gt 99 ]; then
	umask 002
else
	umask 022
fi

# ---------------------------------------------------------
# -- 1.2) Set up bash prompt and ~/.bash_eternal_history --
# ---------------------------------------------------------
#  Set various bash parameters based on whether the shell is 'interactive'
#  or not.  An interactive shell is one you type commands into, a
#  non-interactive one is the bash environment used in scripts.
if [ "$PS1" ]; then

    if [ -x /usr/bin/tput ]; then
      if [ "x`tput kbs`" != "x" ]; then # We can't do this with "dumb" terminal
        stty erase `tput kbs`
      elif [ -x /usr/bin/wc ]; then
        if [ "`tput kbs|wc -c `" -gt 0 ]; then # We can't do this with "dumb" terminal
          stty erase `tput kbs`
        fi
      fi
    fi
    case $TERM in
	xterm*)
		if [ -e /etc/sysconfig/bash-prompt-xterm ]; then
			PROMPT_COMMAND=/etc/sysconfig/bash-prompt-xterm
		else
	    	PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
		fi
		;;
	screen)
		if [ -e /etc/sysconfig/bash-prompt-screen ]; then
			PROMPT_COMMAND=/etc/sysconfig/bash-prompt-screen
		else
		PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\033\\"'
		fi
		;;
	*)
		[ -e /etc/sysconfig/bash-prompt-default ] && PROMPT_COMMAND=/etc/sysconfig/bash-prompt-default

	    ;;
    esac

    # Bash eternal history
    # --------------------
    # This snippet allows infinite recording of every command you've ever
    # entered on the machine, without using a large HISTFILESIZE variable,
    # and keeps track if you have multiple screens and ssh sessions into the
    # same machine. It is adapted from:
    # http://www.debian-administration.org/articles/543.
    #
    # The way it works is that after each command is executed and
    # before a prompt is displayed, a line with the last command (and
    # some metadata) is appended to ~/.bash_eternal_history.
    #
    # This file is a tab-delimited, timestamped file, with the following
    # columns:
    #
    # 1) user
    # 2) hostname
    # 3) screen window (in case you are using GNU screen)
    # 4) date/time
    # 5) current working directory (to see where a command was executed)
    # 6) the last command you executed
    #
    # The only minor bug: if you include a literal newline or tab (e.g. with
    # awk -F"\t"), then that will be included verbatime. It is possible to
    # define a bash function which escapes the string before writing it; if you
    # have a fix for that which doesn't slow the command down, please submit
    # a patch or pull request.
    PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND ; }"'echo -e $$\\t$USER\\t$HOSTNAME\\tscreen $WINDOW\\t`date +%D%t%T%t%Y%t%s`\\t$PWD"$(history 1)" >> ~/.bash_eternal_history'

    # Turn on checkwinsize
    shopt -s checkwinsize

    #Prompt edited from default
    [ "$PS1" = "\\s-\\v\\\$ " ] && PS1="[\u \w]\\$ "

    if [ "x$SHLVL" != "x1" ]; then # We're not a login shell
        for i in /etc/profile.d/*.sh; do
	    if [ -r "$i" ]; then
	        . $i
	    fi
	done
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi

unset color_prompt force_color_prompt
# Make prompt informative
# See: http://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/x329.html
#PS1="\[\033[0;34m\][\u@\h:\w]$\[\033[0m\]"

# Append to history
# See:  http://www.ukuug.org/events/linux2003/papers/bash_tips/
shopt -s histappend

## -----------------------
## -- 2) Set up aliases --
## -----------------------

# 2.1) Safety
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
set -o noclobber

# 2.2) Listing, directories, and motion
alias ll="ls -alrtF --color"
alias md='mkdir'
alias cl='clear'
alias du='du -ch --max-depth=1'
alias treeacl='tree -A -C -L 2'

# 2.4) grep options
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;31' # green for matches

export LC_ALL="en_US.UTF-8"

# 2.6) Install rlwrap if not present
# http://stackoverflow.com/a/677212
command -v rlwrap >/dev/null 2>&1 || { echo >&2 "Install rlwrap to use node: sudo apt-get install -y rlwrap";}

# 2.7) node.js and nvm
# http://nodejs.org/api/repl.html#repl_repl
alias node="env NODE_NO_READLINE=1 rlwrap node"
alias node_repl="node -e \"require('repl').start({ignoreUndefined: true})\""
export NODE_DISABLE_COLORS=1
if [ -s ~/.nvm/nvm.sh ]; then
    NVM_DIR=~/.nvm
    source ~/.nvm/nvm.sh
    nvm use v0.10.12 &> /dev/null # silence nvm use; needed for rsync
fi

## ------------------------------
## -- 3) User-customized code  --
## ------------------------------

## Define any user-specific variables you want here.
#source ~/.bashrc_custom

# Git alias
alias g="git status"
alias ga="git add"
alias gaa="git add ."
alias gau="git add -u"
alias gc="git commit -m"
alias gbd="git branch -d"
alias gch="git checkout"
alias gchb="git checkout -b"
alias gt="git stash"
alias gta="git stash apply"
alias gm="git merge"
alias gr="git rebase"
alias gl="git log --oneline --decorate --graph"
alias gs="git show"
alias gd="git d"
alias gdc="git d --cached"
alias gps="git push"
alias gpl="git pull"

# solarized dark
echo -ne '\e]4;0;#073642\a'   # black
echo -ne '\e]4;1;#dc322f\a'   # red
echo -ne '\e]4;2;#859900\a'   # green
echo -ne '\e]4;3;#b58900\a'   # yellow
echo -ne '\e]4;4;#268bd2\a'   # blue
echo -ne '\e]4;5;#d33682\a'   # magenta
echo -ne '\e]4;6;#2aa198\a'   # cyan
echo -ne '\e]4;7;#eee8d5\a'   # white (light grey really)
echo -ne '\e]4;8;#002b36\a'   # bold black (i.e. dark grey)
echo -ne '\e]4;9;#cb4b16\a'   # bold red
echo -ne '\e]4;10;#586e75\a'  # bold green
echo -ne '\e]4;11;#657b83\a'  # bold yellow
echo -ne '\e]4;12;#839496\a'  # bold blue
echo -ne '\e]4;13;#6c71c4\a'  # bold magenta
echo -ne '\e]4;14;#93a1a1\a'  # bold cyan
echo -ne '\e]4;15;#fdf6e3\a'  # bold white

echo -ne '\e]10;#eee8d5\a'  # foreground
echo -ne '\e]11;#002b36\a'  # background
# echo -ne '\e]12;#859900\a'  # cursor
Term=xterm-256color

alias tmux="TERM=screen-256color-bce tmux"
alias zhremote="ssh zhong@zhongml.cloudapp.net"

export WORKON_HOME=$HOME/virtualenvs
#source /usr/local/bin/virtualenvwrapper.sh
