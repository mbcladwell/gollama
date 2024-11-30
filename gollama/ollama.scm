(define-module (gollama ollama) 
 #:use-module (srfi srfi-19) ;; date time
 #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
 #:use-module (srfi srfi-9)  ;;records
 #:use-module (ice-9 rdelim)
 #:use-module (ice-9 i18n)   ;; internationalization
 #:use-module (ice-9 popen)
 #:use-module (ice-9 regex) ;;list-matches
 #:use-module (ice-9 receive)	     
 #:use-module (ice-9 string-fun)  ;;string-replace-substring
 #:use-module (ice-9 pretty-print)
 #:use-module (json)
 #:use-module (rnrs bytevectors )
;; #:use-module (rnrs io ports #:select ())
 #:use-module (ice-9 textual-ports)
 #:use-module (mcron job-specifier)
 #:use-module (ice-9 ftw);;scandir
 #:use-module (ice-9 format)
 #:export (get-message
	   send-chat
	   ))


(define (get-message lst)
  ;;lst is the list returned by ollama
  (assoc-ref (assoc-ref (vector-ref (assoc-ref lst "choices") 0) "message") "content"))


;;curl http://localhost:11434/v1/chat/completions -d '{"model": "gemma2:2b","messages": [{"role": "user", "content": "Tell me a story about a brave knight"}], "stream": true}'

(define (send-chat text)
  (let* ((command (string-append "curl " *ollama-uri* " -d '{\"model\": \"" *model* "\",\"messages\": [{\"role\":\"user\", \"content\":\"" text "\"}], \"stream\":false}'"))
	 (a (call-command-with-output-to-string command))
	 (b (json-string->scm a))
	 )
    (get-message b) 
  ))
