(define-module (gollama env) 
#:use-module (srfi srfi-19) ;; date time
#:use-module (srfi srfi-98) ;; env vars
#:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
#:use-module (ice-9 rdelim)
#:use-module (ice-9 popen)
#:use-module (ice-9 regex) ;;list-matches
#:use-module (ice-9 pretty-print)
#:use-module (ice-9 binary-ports)
#:use-module (json)
#:use-module (rnrs bytevectors)
#:use-module (rnrs io ports)
;#:use-module (ice-9 textual-ports)
#:use-module (gcrypt base64)
#:export ( 
	  get-envs
	  ))

;;working-dir determined by starting dir

(define *working-dir* (getcwd))
(define *tweet-length* #f)
(define *redirecturi* #f)
(define *data-dir* #f)
(define *platform* #f)
(define *gpg-key* "babweb@build-a-bot.biz")



(define (get-envs dir)
  ;;returns a list
  (if (access?  (string-append dir "/envs") R_OK)
      (let* (
	   ;;  (command  (string-append "gpg --decrypt " dir "/envs"))
	   ;;  (js (call-command-with-output-to-string command))

	     
	     (p  (open-input-file (string-append *working-dir* "/envs")))
	     (a (json-string->scm (get-string-all p)))
	     (dummy (close-port p))
	     )
	a)
      #f))
  


;;guix shell --manifest=manifest.scm -- guile -L /home/mbc/projects/ebbot  -e '(ebbot env)' -s /home/mbc/projects/ebbot/ebbot/env.scm env-clear.txt env.txt

;;guile -L /home/mbc/projects/babweb -e '(ebbot env)' -s /home/mbc/projects/babweb/babweb/lib/env.scm env-clear-template.txt env.txt

;;(define (convert-to-encrypted fin fout)
;; (define (main args)
;;   (let* ((fin (cadr args))
;; 	 (fout (caddr args))
;; 	 (p  (open-input-file fin))
;; 	 (bytes64  (base64-encode (get-bytevector-all p)))
;; 	 (dummy (close-port p))
;; 	 (p2  (open-output-file fout))	
;; 	)	
;;     (put-string p2 bytes64)))

;;guile -L /home/mbc/projects/babweb -e '(ebbot env)' -s /home/mbc/projects/babweb/babweb/lib/env.scm env-clear-template.txt
;; (define (main args)
;;   (let* ((fin (cadr args))
;; 	 (p  (open-input-file fin))
;; 	 (command (string-append "gpg --output envs --encrypt --recipient " *gpg-key* " " fin))
;; 	 (_ (system command))
;; 	 )
;;     (close-port p)
;;     ))

;;with everything in the store, you must place a subdir ebbot with env.scm which then has
;;to be first in GUILE_LOAD_PATH ::   export GUILE_LOAD_PATH="/home/mbc/projects/mastodon/test${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"x
;;guix shell --manifest=manifest.scm -- guile  -e '(ebbot mastodon)' -s /home/mbc/.guix-profile/share/guile/site/3.0/ebbot/mastodon.scm

;;gpg --output envs --encrypt --recipient babweb@build-a-bot.biz env-clear-template.txt
;;gpg --output e-clear.txt --decrypt envs
