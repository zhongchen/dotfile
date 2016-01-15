### setup Mac environment
brew install tmux
brew install macvim
brew install curl git git-flow wget zinc
brew install caskroom/cask/brew-cask
brew install ssh-copy-id
brew cask install iterm2
brew tap caskroom/versions
brew cask install java7
brew cask install sublime-text
brew cask install intellij-idea

# use macvim 
sudo ln -sf /usr/local/bin/mvim /usr/local/bin/vi
sudo ln -sf /usr/local/bin/mvim /usr/local/bin/vim
hash -r

ln -sf ~/dotfile/vimrc ~/.vimrc
ln -sf ~/dotfile/tmux.conf ~/.tmux.conf
#ln -sf ~/dotfile/gitconfig ~/.gitconfig
ln -sf ~/dotfile/vim ~/.vim

