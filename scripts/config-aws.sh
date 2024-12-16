#! /bin/bash

sudo apt-get --assume-yes update
sudo NEEDRESTART_MODE=a apt-get --assume-yes upgrade
sudo apt-get --assume-yes autoremove 
sudo apt-get --assume-yes autoclean
sudo apt-get install nscd gnupg

wget 'https://sv.gnu.org/people/viewgpg.php?user_id=15145' -qO - | sudo -i gpg --import -
wget 'https://sv.gnu.org/people/viewgpg.php?user_id=127547' -qO - | sudo -i gpg --import -

git clone --depth 1 https://github.com/mbcladwell/gollama.git
sudo chown -R $USER ~/gollama

rm ./gollama/envs
cp ./gollama/scripts/envs ./gollama

sudo ./gollama/scripts/guix-install-mod.sh
#guix pull
guix package -i glibc-locales guile gnutls guile-readline guile-json guile-gcrypt            
sudo guix install glibc-locales

export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
GUIX_PROFILE="/home/ubuntu/.guix-profile"
     . "$GUIX_PROFILE/etc/profile"


curl -fsSL https://ollama.com/install.sh | sh

ollama pull llama3.2:1b
ollama pull nomic-embed-text



# scp -i lapan.pem /home/mbc/projects/gollama/scripts/config-aws.sh ubuntu@ec2-54-173-80-215.compute-1.amazonaws.com:.


# scp -i lapan.pem ubuntu@ec2-54-175-236-236.compute-1.amazonaws.com:~/gollama/db/queries-ppan-* /home/mbc/projects/gollama/db/
