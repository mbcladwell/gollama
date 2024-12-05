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
	     (gollama env)	     
	     (gollama utilities)	     
	     (gollama ollama)	     
	     (gollama db)
	     )

;;https://www.youtube.com/watch?v=V1Mz8gMBDMo
;; 8:40 json of embeddings    5:45 embeddings
;; 9:48 has full embeddings   12:00 5 most similar  13:00 bge-base-en-v1.5


;;https://www.youtube.com/watch?v=ztBJqzBU5kc  langchain and embedding
;;https://github.com/ollama/ollama/blob/main/docs/api.md ollama api

(define *top-dir* #f)
(define *chat-uri* #f) 
(define *embeddings-uri* #f) 
(define *model* #f) 
(define *prefix* #f) 
(define *gpg-key* "babweb@build-a-bot.biz")

(define (set-envs varlst)
  (begin
      (set! *chat-uri* (assoc-ref varlst "chat-uri"))
      (set! *embeddings-uri* (assoc-ref varlst "embeddings-uri"))
      (set! *model* (assoc-ref varlst "model"))
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

(define (get-paragraph-for-id conscell paragraphs)
  ;;submit a cons cell ("123" . "0.56477") and get the paragraph for the id
  ;;paragraphs is the file name assumed to be in ./db/
  (let* ((id (assoc-ref conscell "id")))
	  (assoc-ref paragraphs id)))

(define (recurse-paragraphs-for-ids lst paragraphs results)
  ;;result is initially '()
  ;;returns a list of paragraphs
  (if (null? (cdr lst))
      (begin
	(set! results (cons (get-paragraph-for-id (car lst) paragraphs) results))
	results)	
      (begin
	(set! results (cons (get-paragraph-for-id (car lst) paragraphs) results))
	(recurse-paragraphs-for-ids (cdr lst) paragraphs results))))

  

(define (get-top-hits query file N)
  ;;N number of hits desired
    (let* (
	   (sorted-scores (get-sorted-scores query file *embeddings-uri* *model* *top-dir*))
	   (top-5-scores (get-first-n-list sorted-scores 5 0 '()))
	   (p  (open-input-file (string-append *top-dir* "/db/" file)))
	   (haystack (json-string->scm (get-string-all p)))
	   (_ (close-port p))	   
	   (paras (recurse-paragraphs-for-ids top-5-scores haystack '()))
	   )
      paras))


	  

  


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
	 (_  (pretty-print (string-append "in main: " *model*)))
	 ;; (_ (pretty-print (get-first-n-list mylist 10 0 '())))
	 (query "Who is the story's primary villain?")
	 
	;; (query "Where does peter take wendy in the story?")
	 (_  (pretty-print (get-top-hits query "ppan-embeddings-2024120403091733324998.json" 5)))
	 (_ (pretty-print "i am here"))
	;; (_ (pretty-print (get-embedding *embeddings-uri* *model* "sometext" )))
	;; (pretty-print (ingest-doc "/home/mbc/projects/gollama/text/minppan.txt" "1234" *model* *embeddings-uri*))
	 ;;  (ems (get-embeddings uri "mistral" chunk-lst '()))
	 ;;(paragraphs (collect-paragraphs "/home/mbc/projects/gollama/text/minppan.txt"))
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (time-second (time-difference stop-time start-time))))
	 )
    (begin
 ;;     (get-sorted-scores "ppan-embeddings-2024120403091733324998.json")
;;      (pretty-print (cosine-sim #(1 2 3 4 5) #(5 6 7)))
  ;;    (pretty-print (acons 1 "hello" '()) )
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use.")))))


