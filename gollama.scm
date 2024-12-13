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
	     (rnrs sorting) ;;list-sort
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



;; response = ollama.chat { model="mistral" messages=[ "role":"system"
;; 						    "content":"SYSTEM_PROMPT"
;; 						    + "\n".join.paragraphs (most similar chunks)]}  join to the system prompt
;; {"role":"user","content":prompt}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/mbc/projects/gollama")'
;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(main "/home/ubuntu/gollama")'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main args)
  (let* ((start-time (current-time time-monotonic))
	 
	 (_  (pretty-print (string-append "args: " args)))
	 (_ (set-envs (get-envs  args)))
	 (_ (set-envs (get-envs  *top-dir*)))
	 (_  (pretty-print (string-append "in main: " *chat-uri*)))
	 (_  (pretty-print (string-append "in main: " *chat-model*)))
	 ;; (_ (pretty-print (get-first-n-list mylist 10 0 '())))
	 (needle "Who is the story's primary villain?")
	 (haystack "ppan-embeddings-2024120403091733324998.json")
	 (paragraphs "ppan-paragraphs-2024120403091733324998.json")	 
	 ;; (needle "Where does peter take wendy in the story?")	 
	;; (_   (get-top-hits needle haystack  5 paragraphs *embeddings-uri* *model* *top-dir*))   
	;; (_ (pretty-print (get-embedding *embeddings-uri* *model* "sometext" )))
	 ;;(_ (pretty-print (file-sha256 "/home/mbc/projects/gollama/text/minppan.txt")))

;;	 (_ (ingest-doc "/home/mbc/projects/gollama/text/ppan.txt" *embeddings-model* *embeddings-uri* *top-dir* "cosine-sim"))

	 (npara-file (string-append *top-dir* "/db/acbf1a82b78d-npar.json"))
	 (npara-lst   (get-json-from-file npara-file))
;;	 (_ (pretty-print npara-lst))
	 (_ (recurse-process-para "acbf1a82b78d" npara-lst 0 '() '() *embeddings-model* *embeddings-uri* *top-dir*))

	;; (elst '((721 . #(0.0059263664 0.3 0.4  0.5))(722 . #(0.0059263664 0.3 0.4  0.5)) ) )
	;; (_ (pretty-print (scm->json-string (acons "embeddings" (list->vector elst) '()))))
	 ;; (_ (pretty-print (scm->json-string  (list->vector elst) )))
	;; (elst (cons elst '()))
	 (_ (save-to-json (cons elst '()) (string-append *top-dir* "/db/" "jksd97suya5" "-embe.json")))
	 
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (time-second (time-difference stop-time start-time))))
	 )
    (begin
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use."))
      )
    ))


;; In gollama/ollama.scm:
;;    221:24  6 (recurse-process-para _ _ _ _ _ _ _ _)
;;    191:36  5 (get-embedding "http://127.0.0.1:11434/api/embed" _ _)
;; In ice-9/ports.scm:
;;     552:4  4 (call-with-output-string _)
;; In json/builder.scm:
;;     122:6  3 (json-build-object (("model" . #f) ("input" "no?" . #)) ?)
;; In srfi/srfi-1.scm:
;;     634:9  2 (for-each #<procedure 731892ade340 at json/builder.scm?> ?)
;; In json/builder.scm:
;;     121:6  1 (json-build-object ("norm-para" . #("The Project G?" ?)) ?)
;;    105:21  0 (build-object-pair "norm-para" #<output: string 73188f?> ?)

;; json/builder.scm:105:21: In procedure build-object-pair:
;; In procedure car: Wrong type argument in position 1 (expecting pair): "norm-para"
