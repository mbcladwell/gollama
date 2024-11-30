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
	     (gollama env)(gollama utilities)(gollama ollama)
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


;;curl http://localhost:11434/api/embed -d '{"model": "mistral", "input": "Why is the sky blue?"}'

(define (get-embedding text)
  (let* ((command (string-append "curl " "http://localhost:11434/api/embed" " -d '{\"model\": \"" "mistral" "\",\"input\":\"" text "\"}'"))
	 (a (call-command-with-output-to-string command))
	 (b (json-string->scm a))
	 )
    (pretty-print b) 
  ))

;;cosine similariy

(define (square x) (* x x))

(define (cosine-sim x y)
;;cosine similariy
  ;; https://datastax.medium.com/how-to-implement-cosine-similarity-in-python-505e8ec1d823
  ;; x and y must be lists of similar lengths
  (let* ((dot-product (map * x y))
	 (magnitude-x (sqrt (apply + (map square x))))
	 (magnitude-y (sqrt (apply + (map square y))))
	 (dist (* magnitude-x magnitude-y)))
   (apply + (map / dot-product `(,dist ,dist ,dist) ))))


  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/mbc/projects/gollama")'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main args)
  (let* ((start-time (current-time time-monotonic))
	 
	 (_  (pretty-print (string-append "args: " args)))
	 (_ (set-envs (get-envs  args)))
	 (_  (pretty-print (string-append "in main: " *ollama-uri*)))
	 ;; (_ (get-embedding "Hello Mom!"))
	 (x '(5 3 4))
	 (y '(4 2 4))
	 (_ (pretty-print (cossim '(5 3 4)  '(4 2 4 )))) ;;SC(a,b) = 0.989949
	 (_ (pretty-print (map * x y))) ;;SC(a,b) = 0.989949
	;; (_ (pretty-print (map square '(1 2 3) )))
	;; (_ (pretty-print (apply + '(1 2 3) )))
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (time-second (time-difference stop-time start-time))))
	 )
    (begin
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use."))
      )))


