#! /bin/bash

sudo apt-get update
sudo apt-get --assume-yes upgrade 
sudo apt-get --assume-yes autoremove 
sudo apt-get --assume-yes autoclean


#git clone --depth 1 https://github.com/mbcladwell/gollama.git
sudo ./gollama/scripts/guix-install-mod.sh
guix pull
guix package -i glibc-utf8-locales-2.29 guile-json guile-readln guile-gcrypt gnutls
sudo guix install glibc-utf8-locales-2.29

#https://github.com/ollama/ollama/blob/main/docs/faq.md#setting-environment-variables-on-linux

curl -fsSL https://ollama.com/install.sh | sh

ollama pull llama3.2:1b
ollama pull nomic-embed-text
