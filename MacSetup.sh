#!/usr/bin/env bash

brew update

### setup Mac environment
tools=("kubectl" "kubectx" "pyenv" "tmux" "ssh-copy-id" \
  "curl" "git" "wget" "zinc" "percol")

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

if ! [[ -f "/usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]]; then
    brew install zsh-syntax-highlighting
fi

echo "Done"
# brew install macvim
# brew install curl git git-flow wget zinc
# brew install caskroom/cask/brew-cask
# brew cask install iterm2
# brew tap caskroom/versions
# brew cask install sublime-text


# ln -sf ~/dotfile/vimrc ~/.vimrc
# ln -sf ~/dotfile/vim ~/.vim

# # fast grep
# brew install the_silver_searcher
# brew install ripgrep


# brew install python3
# pip3 install --upgrade pip setuptools


# #ruby
# gem install neovim

