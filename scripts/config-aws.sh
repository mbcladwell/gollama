#! /bin/bash

sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get autoremove -y
sudo apt-get autoclean -y
sudo apt-get install guix -y
#guix pull
guix package -i glibc-utf8-locales

#git clone https://github.com/mbcladwell/gollama.git

curl -fsSL https://ollama.com/install.sh | sh

ollama pull llama3.2:1b
ollama pull nomic-embed-text
