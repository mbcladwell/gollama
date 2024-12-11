(add-to-load-path "/home/mbc/projects/gollama")

(use-modules (srfi srfi-19)   ;; date time
	     (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     (srfi srfi-9)  ;;records
	     (srfi srfi-11)
	     (srfi srfi-64)
	     (web response)(web request) (web uri)(web client) (web http)(ice-9 receive)
	     (ice-9 pretty-print)
	     (ice-9 regex) ;;list-matches
	     (ice-9 string-fun)  ;;string-replace-substring	   
	     (ice-9 rdelim)
	     (ice-9 i18n)   ;; internationalization	     	     	     
	     (ice-9 readline)
;;	     (ice-9 iconv)
	     (ice-9 textual-ports)(ice-9 binary-ports)(ice-9 popen)
	     (json)
	     (gcrypt hash)
	     (gcrypt base16)
	     (rnrs bytevectors)
	    (rnrs io ports ) 
	     (gollama env)	     
	     (gollama utilities)	     
	     (gollama ollama)	     
	     (gollama db)
	     (gollama menus)
	     )

;;https://www.youtube.com/watch?v=V1Mz8gMBDMo
;; 8:40 json of embeddings    5:45 embeddings
;; 9:48 has full embeddings   12:00 5 most similar  13:00 bge-base-en-v1.5


;;https://www.youtube.com/watch?v=ztBJqzBU5kc  langchain and embedding
;;https://github.com/ollama/ollama/blob/main/docs/api.md ollama api

(define *top-dir* #f)
(define *chat-uri* #f) 
(define *embeddings-uri* #f) 
(define *chat-model* #f) 
(define *embeddings-model* #f) 
(define *prefix* #f) 
(define *gpg-key* "babweb@build-a-bot.biz")

(define (set-envs varlst)
  (begin
      (set! *chat-uri* (assoc-ref varlst "chat-uri"))
      (set! *embeddings-uri* (assoc-ref varlst "embeddings-uri"))
      (set! *chat-model* (assoc-ref varlst "chat-model"))
      (set! *embeddings-model* (assoc-ref varlst "embeddings-model"))
      (set! *prefix* (assoc-ref varlst "prefix"))
      (set! *top-dir* (assoc-ref varlst "top-dir"))
      ))


;;curl http://localhost:11434/api/embed -d '{"model": "mistral", "input": "Why is the sky blue?"}'
(define system-prompt "You are a helpful reading assistant who answers questions based on snippets of text provided in context. Answer only using the context provided, being as concise as possible. If you are unsure just say that you don't know. Context:")


(define (recurse-get-scores lst out)
  ;;lst is the alist from the embeddings file
  ;;out is the alist from the embedding file
  (if (null? (cdr lst))
      (car lst)
      (my-last (cdr lst))))

;; the alist looks like:
;; ("0"
;;  .
;;  #(0.073323704
;;    0.0071268696
;;    -0.122482516              1684 elements

;; find-most-similar
;;implement minimum length



;; response = ollama.chat { model="nomic-embed-text" messages=[ "role":"system"
;; 						    "content":"SYSTEM_PROMPT"
;; 						    + "\n".join.paragraphs (most similar chunks)]}  join to the system prompt
;; {"role":"user","content":prompt}


;; (define (chat-with-prompt uri model custom-prompt similar-chunks query)
;;   ;;custom-prompt:
;;   ;;similar-chunks:
;;   (let* ((content (string-append system-prompt "\n" similar-chunks))
;; 	  (data `(("model" . ,model)("message" . #((("role" . "system")("content" . ,content))(("role" . "user")("content" . ,query))))
;; 	  (a (receive (response body)
;; 	       (http-request uri
;; 			     #:method 'POST
;; 			     #:body (scm->json-string data)
;; 			     #:streaming? #f
;; 			     #:verify-certificate? #f)
;; 	      (utf8->string body)))	   
;; 	  (b (assoc-ref (json-string->scm a)  "embeddings"))
;; 	  (c (vector-ref b 0))
;; 	)))
;;   ))

	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/mbc/projects/gollama")'
;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(main "/home/ubuntu/gollama")'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main args)
  (let* ((start-time (current-time time-monotonic))
	 
	 (_  (pretty-print (string-append "args: " args)))
	 (_ (set-envs (get-envs  args)))
	 (_  (pretty-print (string-append "in main: " *chat-uri*)))
	 (_  (pretty-print (string-append "in main: " *chat-model*)))
	 ;; (_ (pretty-print (get-first-n-list mylist 10 0 '())))
	 (needle "Who is the story's primary villain?")
	 (haystack "ppan-embeddings-2024120403091733324998.json")
	 (paragraphs "ppan-paragraphs-2024120403091733324998.json")	 
	 ;; (needle "Where does peter take wendy in the story?")	 
	;; (_   (get-top-hits needle haystack  5 paragraphs *embeddings-uri* *model* *top-dir*))   
	;; (_ (pretty-print (get-embedding *embeddings-uri* *model* "sometext" )))
	 ;; (pretty-print (ingest-doc "/home/mbc/projects/gollama/text/minppan.txt" "1234" *model* *embeddings-uri*))
	 ;;(_ (pretty-print (file-sha256 "/home/mbc/projects/gollama/text/minppan.txt")))
	;; (_ (pretty-print (make-doc-list-element  "mytitle" (get-nonce 20 "") "llama32" (date->string  (current-date) "~y~m~d~I~M") )))
	 ;;  (ems (get-embeddings uri "mistral" chunk-lst '()))
	 (a (make-doc-list-element "/home/mbc/projects/gollama/text/ppan.txt" *embeddings-model* "cosine-sim"))
	;; (_ (save-list-to-json "test" a *top-dir* ))
	 ;;		 (_ (add-doc-entry a *top-dir*))
	 (_ (display-logo *top-dir*))
;;	 (_ (pretty-print (get-sha256 (string->bytevector "hello world" (make-transcoder (utf-8-codec))))))
;;	 (_ (pretty-print (get-sha256  "hello world" )))
	 ;;(paragraphs (collect-paragraphs "/home/mbc/projects/gollama/text/minppan.txt"))
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (time-second (time-difference stop-time start-time))))
	 )
    (begin
 ;;     (get-sorted-scores "ppan-embeddings-2024120403091733324998.json")
;;      (pretty-print (cosine-sim #(1 2 3 4 5) #(5 6 7)))
  ;;    (pretty-print (acons 1 "hello" '()) )
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use.")))))


