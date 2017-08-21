### setup Mac environment
brew install tmux
brew install macvim
brew install curl git git-flow wget zinc
brew install caskroom/cask/brew-cask
brew install ssh-copy-id
brew cask install iterm2
brew tap caskroom/versions
brew cask install sublime-text

# use macvim 
#sudo ln -sf /usr/local/bin/mvim /usr/local/bin/vi
#sudo ln -sf /usr/local/bin/mvim /usr/local/bin/vim
hash -r

ln -sf ~/dotfile/vimrc ~/.vimrc
ln -sf ~/dotfile/tmux.conf ~/.tmux.conf
#ln -sf ~/dotfile/gitconfig ~/.gitconfig
ln -sf ~/dotfile/vim ~/.vim

# fast grep
brew install the_silver_searcher
brew install ripgrep

#neovim
brew tap neovim/neovim             # only on first time
brew install neovim/neovim/neovim  # to install last STABLE version

brew install python3
pip3 install --upgrade pip setuptools

pip3 install neovim
# to upgrade
pip3 install --upgrade neovim

#ruby
gem install neovim

