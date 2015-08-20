### setup Mac environment
brew install tmux
brew install macvim

# use macvim 
sudo ln -sf /usr/local/bin/mvim /usr/local/bin/vi
sudo ln -sf /usr/local/bin/mvim /usr/local/bin/vim
hash -r

ln -sf ~/dotfile/vimrc ~/.vimrc
ln -sf ~/dotfile/tmux.conf ~/.tmux.conf
ln -sf ~/dotfile/gitconfig ~/.gitconfig
ln -sf ~/dotfile/vim ~/.vim

