#!/bin/zsh

brew update

### setup Mac environment
tools=("kubectl" "kubectx" "pyenv" "tmux" "ssh-copy-id" \
  "curl" "git" "wget" "zinc" "percol" "kind" "helm" "jq" \
  "npm" "yarn" "node" "go" "volta" "pre-commit" "hub" "luarocks" \
  "virtualenv")

for tool in ${tools[*]}; do
    if ! command -v "${tool}" &> /dev/null
    then
    echo "Installing ${tool}"
    brew install "$tool"
    fi
done


ln -sf ~/dotfile/tmux.conf ~/.tmux.conf
ln -sf ~/dotfile/zshrc ~/.zshrc
ln -sf ~/dotfile/gitconfig ~/.gitconfig
ln -sf ~/dotfile/vimrc ~/.vimrc
# ln -sf ~/dotfile/vim ~/.vim

if ! [[ -f "/usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]]; then
    brew install zsh-syntax-highlighting
fi

declare -A tools_mapping
tools_mapping[rg]=ripgrep
tools_mapping[ag]=the_silver_searcher
tools_mapping[http]=httpie

for key value in ${(kv)tools_mapping}; do
    if ! command -v "${key}" &> /dev/null
    then
    echo "Installing ${value}"
    brew install "$key"
    fi
done

declare -A gui_tools
gui_tools[mvim]=macvim 

for key value in ${(kv)gui_tools}; do
    if ! command -v "${key}" &> /dev/null
    then
    echo "Installing ${value}"
    brew install --cask "$key"
    fi
done


# https://github.com/agnoster/agnoster-zsh-theme/issues/123
# install powerline fonts
# https://github.com/powerline/fonts

echo "Done"
