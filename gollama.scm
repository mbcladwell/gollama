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

(define sys-prompt-short "You are a helpful reading assistant who answers questions based on snippets of text provided in context. Answer only using the context provided, being as concise as possible. If you are unsure just say that you don't know.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/mbc/projects/gollama")'
;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(main "/home/ubuntu/gollama")'
;;guile -L . -l "gollama.scm" -c '(main "/home/ubuntu/gollama")'
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
	;; (haystack "ppan-embeddings-2024120403091733324998.json")
	;; (paragraphs "ppan-paragraphs-2024120403091733324998.json")	 
	 ;; (needle "Where does peter take wendy in the story?")	 
	;; (_   (get-top-hits needle haystack  5 paragraphs *embeddings-uri* *model* *top-dir*))   
	;; (_ (pretty-print (get-embedding *embeddings-uri* *model* "sometext" )))
	 ;;(_ (pretty-print (file-sha256 "/home/mbc/projects/gollama/text/minppan.txt")))
;;       ########## 1 ingest
;;	 (_ (ingest-doc "/home/mbc/projects/gollama/text/ppan.txt" *embeddings-model* *embeddings-uri* *top-dir* "cosine-sim"))

;;       ########## 2 get embeddings
;;	 (npara-file (string-append *top-dir* "/db/acbf1a82b78d-npar.json"))
;;	 (npara-lst   (get-list-from-json-file npara-file))
;;	 (_ (pretty-print npara-lst))
;;	 (_ (recurse-process-para "acbf1a82b78d" npara-lst 0 '() '() *embeddings-model* *embeddings-uri* *top-dir*))

;;	 #######################################
	;; (elst '((721 . #(0.0059263664 0.3 0.4  0.5))(722 . #(0.0059263664 0.3 0.4  0.5)) ) )
	;; (_ (pretty-print (scm->json-string (acons "embeddings" (list->vector elst) '()))))
	 ;; (_ (pretty-print (scm->json-string  (list->vector elst) )))
	;; (elst (cons elst '()))
;;	 (_ (save-to-json (cons elst '()) (string-append *top-dir* "/db/" "jksd97suya5" "-embe.json")))
;;	 #######################################

;;       ########## 3 get paragraphs most similar to queries
	 (q-file (string-append *top-dir* "/db/queries-ppan.json"))
	 (q-lst   (get-list-from-json-file q-file))
;;	 (_ (pretty-print q-lst))
;;	 (_ (recurse-process-para "queries-ppan" q-lst 0 '() '() *embeddings-model* *embeddings-uri* *top-dir*))

	 (query-embeds (get-list-from-json-file (string-append *top-dir* "/db/queries-ppan-embe.json")))
	 (corpus-embeds (get-list-from-json-file (string-append *top-dir* "/db/acbf1a82b78d-embe.json")))	 
	 (queries  (get-list-from-json-file (string-append *top-dir* "/db/queries-ppan-ipar.json")))
	 (q-embed  (get-list-from-json-file (string-append *top-dir* "/db/queries-ppan-embe.json")))
;;	 (_ (pretty-print (assoc-ref queries "1")))
;;	 (_ (pretty-print (assoc-ref q-embed "1"))) ;;<====compare this to all paragraphs embeddings
	 (qem (assoc-ref q-embed "1"))
	 
	 (top5-concat (get-top-hits qem corpus-embeds 5 "acbf1a82b78d-ipar.json" *top-dir*))
;;	 (_ (pretty-print top5-concat))
;;       ########## 4 augment query with RAG
;;        response = ollama.chat { model="mistral" messages=[ "role":"system"
;; 						    "content":"SYSTEM_PROMPT"
;; 						    + "\n".join.paragraphs (most similar chunks)]}  join to the system prompt
	 ;;         {"role":"user","content":prompt}
	 (system-prompt "You are a helpful reading assistant who answers questions based on snippets of text provided in context. Answer only using the context provided, being as concise as possible. If you are unsure just say that you don't know.")

;;	 (system-prompt (string-append system-prompt " Context: " top5-concat))	 
;;	 (prompt "Who is the story's primary villain?")
	 (prompt "Where does Peter take Wendy in the story?")	 

	 (data `(("model" . ,*chat-model*)("messages" . #((("role" . "system")("content" . ,system-prompt))(("role" . "user")("content" . ,prompt))))))

	 (_ (pretty-print (get-chat-response *chat-uri* data)))
	 
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (time-second (time-difference stop-time start-time))))
	 )
    (begin
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use."))
      )
    ))


