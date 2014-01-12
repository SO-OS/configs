# install git, fish
mkdir code; cd code

git clone https://github.com/SO-OS/configs.git

cp configs/.emacs ~/

chsh -s /usr/local/bin/fish
