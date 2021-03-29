# Path to your oh-my-zsh installation.
export ZSH=/Users/$USER/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="agnoster"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(terraform git go docker docker-compose kubectl)

# User configuration
export PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH=/Users/$USER/.local/bin:$PATH

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8


# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

export MVN_HOME=/Users/$USER/apache-maven-3.6.2
export MAVEN_OPTS="-Xmx2g -Djava.awt.headless=true -XX:ReservedCodeCacheSize=512m"
export PATH=/opt/local/bin:/usr/local/bin:$MVN_HOME/bin:$PATH
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_221.jdk/Contents/Home/
export GITHUB=~/Github

# Note: ~/.ssh/environment should not be used, as it
#       already has a different purpose in SSH.

env=~/.ssh/agent.env

# Note: Don't bother checking SSH_AGENT_PID. It's not used
#       by SSH itself, and it might even be incorrect
#       (for example, when using agent-forwarding over SSH).

agent_is_running() {
    if [ "$SSH_AUTH_SOCK" ]; then
        # ssh-add returns:
        #   0 = agent running, has keys
        #   1 = agent running, no keys
        #   2 = agent not running
        ssh-add -l >/dev/null 2>&1 || [ $? -eq 1 ]
    else
        false
    fi
}

agent_has_keys() {
    ssh-add -l >/dev/null 2>&1
}

agent_load_env() {
    . "$env" >/dev/null
}

agent_start() {
    (umask 077; ssh-agent >"$env")
    . "$env" >/dev/null
}

if ! agent_is_running; then
    agent_load_env
fi

# if your keys are not stored in ~/.ssh/id_rsa or ~/.ssh/id_dsa, you'll need
# to paste the proper path after ssh-add
if ! agent_is_running; then
    agent_start
    ssh-add
elif ! agent_has_keys; then
    ssh-add
fi

unset env

function exists { which $1 &> /dev/null }

# Git alias
alias g="git status"
alias ga="git add"
alias gaa="git add ."
alias gau="git add -u"
alias gc="git commit -m"
alias gcamend="git commit --amend --no-edit"
alias gbd="git branch -D"
alias gb="git branch | peco | xargs git checkout"
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

Term=xterm-256color
alias tmux="TERM=screen-256color-bce tmux"

# 2.1) Safety
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
set -o noclobber

mount-encrypted()
{
  hdiutil attach ~/Dropbox/encrypted.dmg -readonly -stdinpass
}

mount-encrypted-rw()
{
  hdiutil attach ~/Dropbox/encrypted.dmg -stdinpass
}

unmount-encrypted()
{
  diskutil eject /Volumes/Encrypted
}

mount-encrypted-personal()
{
  hdiutil attach ~/Dropbox/personal_encrypted.dmg -readonly -stdinpass
}

mount-encrypted-personal-rw()
{
  hdiutil attach ~/Dropbox/personal_encrypted.dmg -stdinpass
}

unmount-encrypted-personal()
{
  diskutil eject /Volumes/PersonalEncrypted
}

alias sumojira='gl --author="Zhong Chen" --pretty=format:%s4m | grep -oE "^SUMO-[0-9]+" | head -1 | pbcopy'
alias deletemergedbranches='git branch --merged | grep -v "\*" | grep -v "master" | xargs -n 1 git branch -d'
alias push-current-branch="~/scripts/bin/push-current-branch.sh"

# Run an individual unit test when in the relevant module directory eg:
# unittest ReliableCuratorLockTest
function unittest() {
  #mvn test -P zinc -T 1C -Dsumo.suites=all -Dtest=$1
  mvn test -P zinc -T 1C -DredirectTestOutputToFile=false -Dsumo.suites=all -Dtest=$1
}

alias ctags="`brew --prefix`/bin/ctags"

export PIP_REQUIRE_VIRTUALENV=false

remove-fatal-warnings()
{
  sed -i '' '/fatal-warnings/d' .idea/scala_compiler.xml
}

export ENC_DRIVE_ROOT=/Volumes/Encrypted

pr () {
    if [ -n "$1" ]; then
        MERGE_BRANCH=$1
    else
        MERGE_BRANCH="master"
    fi
    CURRENT_BRANCH=`git rev-parse --abbrev-ref HEAD`
    REPO=`git remote -v | grep push | awk -F':' '{print $2}' | awk -F'.' '{print $1}'`
    open "https://github.com/$REPO/compare/$MERGE_BRANCH...$CURRENT_BRANCH"
}

export QUICK_ASSEMBLE_NO_SNAPSHOTS=true
export QUICK_ASSEMBLE_SKIP_NEXUS_CHECK=true

export GOPATH="/Users/$USER/go"
export GOROOT="/usr/local/opt/go/libexec/"
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

function frameworkpython {
    if [[ ! -z "$VIRTUAL_ENV" ]]; then
        PYTHONHOME=$VIRTUAL_ENV /usr/local/bin/python "$@"
    else
        /usr/local/bin/python "$@"
    fi
}

# Search in all files matching `.log` under the given directory
alias logsearch='find | grep "\.log$" | xargs grep'

# # Tail all files matching `.log` under the given directory
alias logtail='find | grep "\.log$" | xargs tail -f'

export TERM=xterm-256color
export EDITOR=vim

 # Use Homebrew's directories instead of ~/.jenv for configs. This is optional.
export JENV_ROOT=/usr/local/var/jenv
 # # Enable shims and autocompletion for jenv.
if exists jenv;
then
 eval "$(jenv init - --no-rehash)";
 (jenv rehash &) 2> /dev/null
fi

function ppgrep() {
    if [[ $1 == "" ]]; then
        PECO=peco
    else
        PECO="peco --query $1"
    fi
    ps aux | eval $PECO | awk '{ print $2 }'
}

function ppkill() {
    if [[ $1 =~ "^-" ]]; then
        QUERY=""            # options only
    else
        QUERY=$1            # with a query
        [[ $# > 0 ]] && shift
    fi
    ppgrep $QUERY | xargs kill $*
}

if exists peco; then
    function peco_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(fc -l -n 1 | eval $tac | peco --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }

    zle -N peco_select_history
    bindkey '^R' peco_select_history
fi

function pattach() {
    if [[ $1 == "" ]]; then
        PECO=peco
    else
        PECO="peco --query $1"
    fi

    sessions=$(tmux ls)
    [ $? -ne 0 ] && return

    session=$(echo $sessions | eval $PECO | cut -d : -f 1)
    if [[ -n "$session" ]]; then
        tmux att -t $session
    fi
}

till_failure () {
    if [ $# -le 0 ]
    then
        echo "Usage: $0 <cmd> <repeatAmount>"
        return -1
    fi
    local REPEAT_CMD="$1"
    local REPEAT_AMOUNT=9999
    if [ $# -gt 1 ]
    then
        REPEAT_AMOUNT=$2
    fi
    local I=0
    local EXIT_CODE=0
    while [[ $I -lt $REPEAT_AMOUNT ]] && [[ $EXIT_CODE -eq 0 ]]
    do
        I=$((I + 1))
        echo "Executing repetition ${I}..."
        eval $REPEAT_CMD
        EXIT_CODE=$?
    done
    return $EXIT_CODE
}

alias vim="/usr/local/bin/vim"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

PATH="/Users/zhong/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/zhong/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/zhong/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/zhong/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/zhong/perl5"; export PERL_MM_OPT;

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

function extract()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1     ;;
            *.tar.gz)    tar xvzf $1     ;;
            *.bz2)       bunzip2 $1      ;;
            *.rar)       unrar x $1      ;;
            *.gz)        gunzip $1       ;;
            *.tar)       tar xvf $1      ;;
            *.tbz2)      tar xvjf $1     ;;
            *.tgz)       tar xvzf $1     ;;
            *.zip)       unzip $1        ;;
            *.Z)         uncompress $1   ;;
            *.7z)        7z x $1         ;;
            *)           echo "'$1' cannot be extracted via >extract<" ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}


export PIPENV_VENV_IN_PROJECT="enabled"

# pyenv setup
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
if which pyenv-virtualenv > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi


export AIRFLOW_HOME=~/airflow
export SLUGIFY_USES_TEXT_UNIDECODE=yes

function push-current-branch()
{
    sh ~/Bitbucket/scripts/push-current-branch.sh
}

function memory-analyzer()
{
  ~/tools/mat/MemoryAnalyzer.app/Contents/MacOS/MemoryAnalyzer -vmargs -Xmx16g -XX:-UseGCOverheadLimit
}

function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export NO_NEED_CDJENKINS=TRUE

export PATH=~/mongodb/bin:$PATH

alias airflow-node="ssh -i ~/hackthon/datawarehouse.pem ubuntu@ec2-3-93-239-147.compute-1.amazonaws.com"
alias app-server-node="ssh -i ~/hackthon/datawarehouse.pem ubuntu@ec2-3-84-55-78.compute-1.amazonaws.com"

alias docker-stop-all="docker stop $(docker ps -a -q)"
alias docker-rm-all="docker rm $(docker ps -a -q)"

export PATH=$HOME/bin:$PATH
if [ /usr/local/bin/kubectl ]; then source <(kubectl completion zsh); fi

alias kbl='kubectl'

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

DEFAULT_USER=$(whoami)


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/zhongchen/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/zhongchen/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/zhongchen/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/zhongchen/google-cloud-sdk/completion.zsh.inc'; fi

alias tf="terraform"

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /Users/zhongchen/bin/terraform terraform



function start-dataproc () {
 pushd ~/github/zhongchen/dataproc-kedro-demo
 ./start-dataproc.sh $@
 popd
}

