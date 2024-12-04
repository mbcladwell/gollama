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
 #:use-module (web response)
 #:use-module (web request)
 #:use-module (web uri)
 #:use-module (web client)
 #:use-module (web http)
 #:use-module (ice-9 receive)
 #:use-module (ice-9 textual-ports)
 #:use-module (ice-9 ftw);;scandir
 #:use-module (ice-9 format)
 #:use-module (gollama utilities)
 #:export (get-message
	   send-chat
	   cosine-sim
	   collect-paragraphs
	   get-embedding
;;	   recurse-get-embedding
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

(define (square x) (* x x))

(define (cosine-sim x y)
;;cosine similariy
  ;; https://datastax.medium.com/how-to-implement-cosine-similarity-in-python-505e8ec1d823
  ;; x and y must be lists of similar lengths
  (let* ((dot-product (map * x y))
	 (magnitude-x (sqrt (apply + (map square x))))
	 (magnitude-y (sqrt (apply + (map square y))))
	 (dist-lst (make-list (length x) (* magnitude-x magnitude-y))))
   (apply + (map / dot-product dist-lst))))


(define (clean-chars s)
  ;;remove offensive characters
  (let* ((out (string-replace-substring s "'" ""))
	 (out (string-replace-substring out "’" ""))
	 (out (string-replace-substring out "\"" ""))
	 (out (string-replace-substring out "“" ""))
	 (out (string-replace-substring out "”" ""))
	 (out (string-replace-substring out "…" ""))
	 )
    (string-trim-both out #\space)))


;; (define (collect-paragraphs text)
;;   (begin
;;     (define paragraph "")
;;     (define para-lst '())
;;     (let* (
;; ;;	   (text "/home/mbc/projects/gollama/text/ppan.txt")
;; 	   (port (open-input-file text))
;; 	   (line  (read-line port))
;; 	   (_ (while (not (eof-object? line))		    
;; 		(if (= 0 (string-length line))
;; 		    (begin
;; 		      (set! para-lst (cons paragraph para-lst))
;; 		      (set! paragraph "")
;; 		      (set! line (read-line port))
;; 		      )
;; 		    (begin
;; 		      (set! paragraph (clean-chars (string-append paragraph " " line )))
;; 		      (set! line (read-line port))))
;; 		)))  ;; end of empty line check
;; 	   (cons paragraph para-lst)) ;;is EOF but must append last paragraph
;; 	   ))

(define (collect-paragraphs text)  
  (define paragraph "")
  (define para-lst '())
  (let* (
	 (port (open-input-file text))
	 (line  (read-line port))
	 (_ (while (not (eof-object? line))	       
	      (while (< 0 (string-length line))		    
		(set! paragraph (clean-chars (string-append paragraph " " line )))
		(set! line (read-line port)))
	      
	      (if (< 0 (string-length paragraph))
		  (begin
		    (set! para-lst (cons paragraph para-lst))
		    (set! paragraph "")))
	      (set! line (read-line port)))));; end of empty line check
    (begin
      (if (< 0 (string-length paragraph))(cons paragraph para-lst))
      para-lst) ;;is EOF but must append last paragraph
    ))


;; (define (get-embedding model-name chunk)
;;   ;;model mistral
;;   (let* ((command (string-append "curl " "http://localhost:11434/api/embed" " -d '{\"model\": \""  model-name  "\",\"input\":\"" chunk "\"}'"))
;; 	 (a (call-command-with-output-to-string command))
;; 	 (_ (pretty-print chunk))
;; 	 (b (assoc-ref (json-string->scm a)  "embeddings")))
;;     (vector-ref b 0)))
;;     b ))

(define (get-embedding model chunk)
  (let* ((a (receive (response body)
	       (http-request "http://127.0.0.1:11434/api/embed"
			     #:method 'POST
			     #:body (scm->json-string `(("model" . ,model)("input" . ,chunk)))
			     #:streaming? #f)
;;			     #:verify-certificate? #f)
	     (utf8->string body)))
	 (b (assoc-ref (json-string->scm a)  "embeddings")))
    (vector-ref b 0)))


(define (recurse-get-embedding model lst out)
  ;;lst is the input list of text chunks
  ;;out is the output list of embeddings
  ;; (get-embeddings "mistral" chunk-lst '())
  (if (null? (cdr lst))
      (begin 
	(set! out (cons (get-embedding model (car lst)) out))
	out)
      (begin
	(set! out (cons (get-embedding model (car lst)) out))
	(get-embeddings model (cdr lst) out))))
