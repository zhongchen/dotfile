ln -sf ~/dotfile/bashrc ~/.bashrc
ln -sf ~/dotfile/bash_profile ~/.bash_profile
ln -sf ~/dotfile/vimrc ~/.vimrc
ln -sf ~/dotfile/tmux.conf ~/.tmux.conf
ln -sf ~/dotfile/zshrc ~/.zshrc
ln -sf ~/dotfile/gitconfig ~/.gitconfig

#set up the folder of vim
#ln -sf ~/dotfie/vim/ ~/.vim/

# solve vim clipboard problem in ubuntu
sudo apt-get install -y vim-gnome
sudo apt-get install -y curl  vim  zsh ctags git-core pip
sudo apt-get install python-pip python-dev build-essential

sudo apt-get install -y python-pip python-dev build-essential
sudo pip install --upgrade pip
sudo pip install --upgrade virtualenv


