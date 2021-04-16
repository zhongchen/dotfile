#!/usr/bin/env bash

brew update

### setup Mac environment
tools=("kubectl" "kubectx" "pyenv" "tmux" "ssh-copy-id" \
  "curl" "git" "wget" "zinc" "percol" "kind" "helm")

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
ln -sf ~/dotfile/vim ~/.vim

if ! [[ -f "/usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]]; then
    brew install zsh-syntax-highlighting
fi



# https://github.com/agnoster/agnoster-zsh-theme/issues/123
# install powerline fonts
# https://github.com/powerline/fonts

echo "Done"



# # fast grep
# brew install the_silver_searcher
# brew install ripgrep



