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
	     (gollama textman)
	     (gollama menus)
	     (rnrs sorting) ;;list-sort
	     )

;;https://www.youtube.com/watch?v=V1Mz8gMBDMo
;; 8:40 json of embeddings    5:45 embeddings
;; 9:48 has full embeddings   12:00 5 most similar  13:00 bge-base-en-v1.5


;;https://www.youtube.com/watch?v=ztBJqzBU5kc  langchain and embedding
;;https://github.com/ollama/ollama/blob/main/docs/api.md ollama api

(define *top-dir* (getcwd))
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
      ))


;;curl http://localhost:11434/api/embed -d '{"model": "mistral", "input": "Why is the sky blue?"}'
(define system-prompt "You are a helpful reading assistant who answers questions based on snippets of text provided in context. Answer only using the context provided, being as concise as possible. If you are unsure just say that you don't know. Context:")

(define sys-prompt-short "You are a helpful reading assistant who answers questions based on snippets of text provided in context. Answer only using the context provided, being as concise as possible. If you are unsure just say that you don't know.")


(define (ingest file)
  ;;########## 1 ingest
  ;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(ingest "cr2025mod.txt")'
  ;;guile -L . -l "gollama.scm" -c '(ingest "ppan.txt" "/home/ubuntu/gollama")'
  (begin
    (set-envs (get-envs (getcwd)))
    (ingest-doc file *chat-model* *embeddings-model* *embeddings-uri* *top-dir* "cosine-sim")))

(define (retrieve-embeddings id)
  ;;########## 2 get embeddings
  ;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(retrieve-embeddings "a5b2fec2ad1f")'
  ;;guile -L . -l "gollama.scm" -c '(retrieve-embeddings "e210c4f856f0")'
  (let* ((_  (set-envs (get-envs (getcwd))))
	 (db-path (string-append *top-dir* "/db/" id "/"))
	 (ipara-file (string-append db-path "indexed-paragraphs.json"))
	 (ipara-lst   (get-list-from-json-file ipara-file #t))
	 (embeddings (recurse-process-para id ipara-lst '() *embeddings-model* *embeddings-uri* *top-dir*))
	 (_ (save-to-json embeddings (string-append db-path "embedded-paragraphs.json") #t))

	 (sys-prompt-file (string-append db-path "indexed-system-prompts.json"))
	 (sys-p-flag (access? sys-prompt-file F_OK))
	 (_ (if sys-p-flag (let* ((a (get-list-from-json-file sys-prompt-file #t))
				;;  (_ (pretty-print a))
				  (b (recurse-process-para id  a '() *embeddings-model* *embeddings-uri* *top-dir*))
				;;  (_ (pretty-print b))
				  )			     			       
			     (save-to-json b (string-append db-path "embedded-system-prompts.json" ) #t)
			       )))

	 (queries-file (string-append db-path "indexed-queries.json"))
	 (q-flag (access? queries-file F_OK))
	 (_ (if q-flag (let* ((a (get-list-from-json-file queries-file #t))
			      (b (recurse-process-para id a '() *embeddings-model* *embeddings-uri* *top-dir*))
				  )			     			       
			     (save-to-json b (string-append db-path "embedded-queries.json" ) #t)
			     )))
	 ;; (contexts-file (string-append db-path "indexed-contexts.json"))
	 ;; (q-flag (access? contexts-file F_OK))
	 ;; (_ (if q-flag (let* ((a (get-list-from-json-file contexts-file #t))
	 ;; 		      (b (recurse-process-para id a '() *embeddings-model* *embeddings-uri* *top-dir*))
	 ;; 			  )			     			       
	 ;; 		     (save-to-json b (string-append db-path "embedded-queries.json" ) #t)
	 ;; 		     )))
	 
	 )
    #f))

(define (match-query-paragraphs id)
  ;;########## 3 get paragraphs most similar to queries
  ;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(match-query-paragraphs "a5b2fec2ad1f")'
  ;;guile -L . -l "gollama.scm" -c '(match-query-paragraphs "a5b2fec2ad1f")'
  (let* ((_  (set-envs (get-envs (getcwd))))
	 (db-path (string-append *top-dir* "/db/" id "/"))
	 (query-embeds (get-list-from-json-file (string-append db-path "embedded-queries.json") #t))
	 (corpus-embeds (get-list-from-json-file (string-append db-path "embedded-paragraphs.json") #t))
;;	 (_ (pretty-print (string-append (length corpus-embeds))))
	 (queries  (get-list-from-json-file (string-append db-path "indexed-queries.json") #t))
	 ;;(_ (pretty-print (assoc-ref queries "1")))
;;	 (_ (pretty-print (assoc-ref query-embeds "1"))) ;;<====compare this to all paragraphs embeddings
	 (qem (assoc-ref query-embeds "1"))
	 
	 (top-n-concat (get-top-hits qem corpus-embeds 5 id *top-dir*))
	 (indexed-contexts (string-append db-path "indexed-contexts.json"))
	 )    
    (begin
      ;;(pretty-print top5-concat)
      (save-to-json top-n-concat (string-append db-path "indexed-contexts.json" ) #t)
      )
  ))

  

(define (query-with-rag id sys-prompt-id context-id query-id )
  ;;id: db directory id
  ;;########## 4 augment query with RAG
  ;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(query-with-rag "43933d20019d" "1" "1" "1")'
  ;;guile -L . -l "gollama.scm" -c '(query-with-rag "a5b2fec2ad1f" "1" "1" "1")'
  ;;response = ollama.chat { model="mistral" messages=[ "role":"system"
  ;; 						    "content":"SYSTEM_PROMPT"
  ;; 						    + "\n".join.paragraphs (most similar chunks)]}  join to the system prompt
  ;;         {"role":"user","content":prompt}
  (let* ((_  (set-envs (get-envs (getcwd))))
	 (db-path (string-append *top-dir* "/db/" id "/"))
	 (context (assoc-ref (get-list-from-json-file (string-append db-path "indexed-contexts.json") #t) context-id))
	;; (_ (pretty-print context))
	 (sys-prompt (assoc-ref (get-list-from-json-file (string-append db-path "indexed-system-prompts.json") #t) sys-prompt-id))
	 (query (assoc-ref (get-list-from-json-file (string-append db-path "indexed-queries.json") #t) query-id))
	 (system-prompt (string-append sys-prompt " Context: " context))	 
	 
;;	 (data `(("model" . ,*chat-model*)("messages" . #((("role" . "system")("content" . ,system-prompt))(("role" . "user")("content" . ,query))))))
	 (data `(("model" . ,*chat-model*)("messages" . #((("role" . "system")("content" . ,system-prompt))(("role" . "user")("content" . ,query))))
		 ("options" . #((("num_ctx" . 4096))))))
	 )
;;    (pretty-print (scm->json-string data))
    (pretty-print (get-chat-response *chat-uri* data))
    ))
      

(define (combine-all-paragraphs file id)
  ;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(combine-all-paragraphs "cr2025mod.txt" "43933d20019d")'
  (let* ((_  (set-envs (get-envs (getcwd))))
	 (db-path (string-append *top-dir* "/db/" id "/"))
	 (the-file (string-append db-path "/" file))
	 (paragraphs (collect-paragraphs the-file))
	 (norm-para (normalize-para-lengths paragraphs 30 '() '()))
	 (norm-para-concat (string-concatenate norm-para))
	 (alst (acons "1" norm-para-concat '()))
	 (dest-file (string-append db-path "/indexed-contexts.json" ))
	 (p  (open-output-file dest-file))
	 (_ (put-string p (scm->json-string alst))))
    (force-output p)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/mbc/projects/gollama")'
;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(main "/home/ubuntu/gollama")'
;;guile -L . -l "gollama.scm" -c '(main "/home/ubuntu/gollama")'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main args)
  (let* ((start-time (current-time time-monotonic))
	 
	 (_  (pretty-print (string-append "args: " args)))
	 (_ (set-envs (get-envs (getcwd))))
	;; (_ (set-envs (get-envs  *top-dir*)))
	 ;;(_  (pretty-print (string-append "in main: " *chat-uri*)))
	 ;;(_  (pretty-print (string-append "in main: " *chat-model*)))
	 ;; (_ (pretty-print (get-first-n-list mylist 10 0 '())))
	;; (_ (pretty-print (get-embedding *embeddings-uri* *model* "sometext" )))
	 
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (time-second (time-difference stop-time start-time))))
	 )
    (begin
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use."))
      )
    ))


