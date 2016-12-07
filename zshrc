# Path to your oh-my-zsh installation.
export ZSH=/Users/zhong/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

# User configuration

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

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

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#
export DEV_HOME=~/Development
export SUMO_HOME=~/Development/sumo
alias dsh="${SUMO_HOME}/ops/bin/dsh.sh"
alias props="${SUMO_HOME}/system/bin/local-props-updater.py"
export MVN_HOME=/Users/zhong/Development/sumo/bin/apache-maven-3.3.3
export MAVEN_OPTS="-Xmx1024m -Djava.awt.headless=true"
export PATH=/opt/local/bin:/usr/local/bin:$MVN_HOME/bin:$PATH
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_79.jdk/Contents/Home/

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

ulimit -n 65536 65536

# Git alias
alias g="git status"
alias ga="git add"
alias gaa="git add ."
alias gau="git add -u"
alias gc="git commit -m"
alias gcamend="git commit --amend --no-edit"
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

alias sumobuild=${SUMO_HOME}/bin/quick-assemble.sh -c -q
alias sumocleanbuild=${SUMO_HOME}/bin/quick-assemble.sh
alias sumotest=${SUMO_HOME}/bin/quick-assemble.sh -t
alias sumopull=${SUMO_HOME}/bin/pull.sh
alias sumopush=${SUMO_HOME}/bin/push-current-branch.sh
alias sumochanges=${SUMO_HOME}/bin/find-uncommitted-changes.sh
alias sumodirparse="cd $SUMO_HOME/parse/src/main/scala/com/sumologic/scala/parse/learn/"
alias sumodirparsetest="cd $SUMO_HOME/parse/src/test/scala/com/sumologic/scala/parse/learn/"
alias sumopropsupdate='$SUMO_HOME/system/bin/local-props-updater.py'
alias sumojira='gl --author="Zhong Chen" --pretty=format:%s4m | grep -oE "^SUMO-[0-9]+" | head -1 | pbcopy'
alias deletemergedbranches='git branch --merged | grep -v "\*" | grep -v "master" | xargs -n 1 git branch -d'
alias sumopursedeployment='dsh -d zhc dep stop -v'
alias start-dynamo="$SUMO_HOME/system/bin/third-party/start-dynamodb.sh"
alias stop-dynamo="$SUMO_HOME/system/bin/third-party/stop-dynamodb.sh"

# Run an individual unit test when in the relevant module directory eg:
# unittest ReliableCuratorLockTest
function unittest() {
  #mvn test -P zinc -T 1C -Dsumo.suites=all -Dtest=$1
  mvn test -P zinc -T 1C -DredirectTestOutputToFile=false -Dsumo.suites=all -Dtest=$1
}

# Start sumologic locally
sumoup() {
  if $SUMO_HOME/system/bin/pre-integration-tests.sh -z 10 $@ ; then
      notify "Sumo" "Launcher" "Sumo is running! ($@)"
  else
      notify "Sumo" "Launcher" "Sumo failed to launch: $@"
      return 1
  fi
}

# Shut down local deployment
alias sumodown=$SUMO_HOME/system/bin/post-integration.sh

# Start one component of sumo locally (after killing it)
alias sumouponly='up -sn -z 0 -o'

# Stop one component of sumo locally
sumodownonly() {
  cd $SUMO_HOME/system
  bin/killProcess.sh com.sumologic.$1.Bootstrap
  bin/killProcess.sh com.sumologic.$1.scala.Bootstrap
  bin/killProcess.sh com.sumologic.scala.$1.Bootstrap
  bin/killProcess.sh com.sumologic.scala.$1.Collector
  bin/killProcess.sh "MODULE=$1"
}


alias ctags="`brew --prefix`/bin/ctags"
alias datarepositoryfactory='cd /Users/zhong/Development/analytics-experiments/DataRepositoryfactory'
alias dataretriever='cd /Users/zhong/Development/analytics-experiments/DataRetriever'


export PIP_REQUIRE_VIRTUALENV=false
#export PIP_REQUIRE_VIRTUALENV=true
gpip() {
  PIP_REQUIRE_VIRTUALENV="" pip "$@"
}

#set up for virtual env wrapper
source /usr/local/bin/virtualenvwrapper.sh
export WORKON_HOME=~/Experiment
export PROJECT_HOME=~/Experiments

remove-fatal-warnings()
{
  sed -i '' '/fatal-warnings/d' .idea/scala_compiler.xml
}

export ENC_DRIVE_ROOT=/Volumes/Encrypted
alias sublime='open -a /opt/homebrew-cask/Caskroom/sublime-text/2.0.2/Sublime\ Text\ 2.app/Contents/MacOS/Sublime\ Text\ 2'

pr () {
    if [ -n "$1" ]
    then
        MERGE_BRANCH=$1 
    else
        MERGE_BRANCH="master" 
    fi
    CURRENT_BRANCH=`git rev-parse --abbrev-ref HEAD` 
    open "https://github.com/Sanyaku/sumologic/compare/$MERGE_BRANCH...$CURRENT_BRANCH"
}

export QUICK_ASSEMBLE_NO_SNAPSHOTS=true
export QUICK_ASSEMBLE_SKIP_NEXUS_CHECK=true

export GOPATH="/Users/zhong/go"
export GOROOT="/usr/local/Cellar/go/1.6/libexec/"
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
