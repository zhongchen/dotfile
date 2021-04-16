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


ln -sf ~/dotfile/zshrc ~/.zshrc

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

# # use macvim 
# #sudo ln -sf /usr/local/bin/mvim /usr/local/bin/vi
# #sudo ln -sf /usr/local/bin/mvim /usr/local/bin/vim
# hash -r

# ln -sf ~/dotfile/vimrc ~/.vimrc
# ln -sf ~/dotfile/tmux.conf ~/.tmux.conf
# #ln -sf ~/dotfile/gitconfig ~/.gitconfig
# ln -sf ~/dotfile/vim ~/.vim

# # fast grep
# brew install the_silver_searcher
# brew install ripgrep

# #neovim
# brew tap neovim/neovim             # only on first time
# brew install neovim/neovim/neovim  # to install last STABLE version

# brew install python3
# pip3 install --upgrade pip setuptools

# pip3 install neovim
# # to upgrade
# pip3 install --upgrade neovim

# #ruby
# gem install neovim

