#! /bin/bash

sudo apt-get update
sudo apt-get --assume-yes upgrade 
sudo apt-get --assume-yes autoremove 
sudo apt-get --assume-yes autoclean
sudo apt-get install nscd

#git clone --depth 1 https://github.com/mbcladwell/gollama.git
rm ./gollama/envs
cp ./gollama/scripts/envs ./gollama

sudo ./gollama/scripts/guix-install-mod.sh
guix pull
guix package -i glibc-locales             
sudo guix install glibc-locales

export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"

curl -fsSL https://ollama.com/install.sh | sh

ollama pull llama3.2:1b
ollama pull nomic-embed-text



