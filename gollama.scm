(add-to-load-path "/home/mbc/projects/gollama")

(use-modules (srfi srfi-19)   ;; date time
	     (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     (srfi srfi-9)  ;;records
	     (web response)(web request) (web uri)(web client) (web http)(ice-9 receive)
	     (ice-9 pretty-print)
	     (ice-9 regex) ;;list-matches
	     (ice-9 string-fun)  ;;string-replace-substring	   
	     (ice-9 rdelim)
	     (ice-9 i18n)   ;; internationalization	     	     	     
	     (ice-9 readline)
	     (ice-9 iconv)
	     (ice-9 textual-ports)(ice-9 binary-ports)(ice-9 popen)
	     (json)
	     (rnrs bytevectors)
	     (rnrs io ports ) ;;make-transocder
	     (gollama env)(gollama utilities)
	     )

;;https://www.youtube.com/watch?v=V1Mz8gMBDMo
;;https://www.youtube.com/watch?v=ztBJqzBU5kc  langchain and embedding
;;https://github.com/ollama/ollama/blob/main/docs/api.md ollama api

(define *working-dir* #f)
(define *data-dir* #f)
(define *ollama-uri* #f) 
(define *model* #f) 
(define *encodings* #f) 
(define *prefix* #f) 
(define *gpg-key* "babweb@build-a-bot.biz")

(define (set-envs varlst)
  (begin
      (set! *ollama-uri* (assoc-ref varlst "ollama-uri"))
      (set! *model* (assoc-ref varlst "model"))
      (set! *encodings* (assoc-ref varlst "encodings"))
      (set! *prefix* (assoc-ref varlst "prefix"))
      (set! *working-dir* (assoc-ref varlst "working-dir"))
      ))


;;curl http://localhost:11434/v1/chat/completions -d '{"model": "gemma2:2b","messages": [{"role": "user", "content": "Tell me a story about a brave knight"}
;;										       ], "stream": true}'

(define (send-chat text)
  (let* ((command (string-append "curl " *ollama-uri* " -d '{\"model\": \"" *model* "\",\"messages\": [{\"role\": \"user\", \"content\": \"" text "\" ], \"stream\": true}'"))
	 (a (call-command-with-output-to-string command))
	 )
   (pretty-print a ) )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/mbc/projects/gollama")'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main args)
  (let* ((start-time (current-time time-monotonic))
	 
	 (_  (pretty-print (string-append "args: " args)))
	 (_ (set-envs (get-envs  args)))
	 (_  (pretty-print (string-append "in main: " *ollama-uri*)))
	 (_ (send-chat "Hello Mom!"))
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (time-second (time-difference stop-time start-time))))
	 )
    (begin
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use."))
      )))


