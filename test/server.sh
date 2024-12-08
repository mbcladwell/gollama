#! /bin/bash
guix shell guile-next guile-ares-rs -- guile -L . -L /home/mbc/projects/gollama -c '((@ (ares server) run-nrepl-server))'

