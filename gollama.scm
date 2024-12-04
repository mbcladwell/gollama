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
	     (rnrs sorting) ;;list-sort
	     )

;;https://www.youtube.com/watch?v=V1Mz8gMBDMo
;; 8:40 json of embeddings    5:45 embeddings

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



;; (define my-list '(
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 1))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 5))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 3))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 6))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 2))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 4))
;; 		  ))

(define (sort-embeddings x y)
  ;;https://www.gnu.org/software/guile/manual/html_node/rnrs-sorting.html
  (> (assoc-ref x "embedding")(assoc-ref y "embedding")))

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



(define (get-sorted-scores file)
  (define counter 0)
  (define results '())
    (let* (
	   (p  (open-input-file (string-append *top-dir* "/db/" file)))
	   (haystack (json-string->scm (get-string-all p)))
	   (dummy (close-port p))
	   (haystack-length (length haystack))
	   (query "Who is Captain Hook?")
	   ;;  (needle (get-embedding *embeddings-uri* *model* query))
	   (needle (assoc-ref haystack "100"))
	  ;; (score (cosine-sim needle (assoc-ref haystack counter)))
	   (_ (while (> haystack-length counter)
	    	(begin
		  (set! results (cons `(("id" . ,(number->string counter))("embedding" . ,(cosine-sim needle (assoc-ref haystack (number->string counter))))) results))
		  
	    	  (set! counter (+ 1 counter)))))
	   (results-sorted (list-sort sort-embeddings results))
	   (p  (open-input-file (string-append *top-dir* "/db/" "ppan-paragraphs-2024120403091733324998.json")))
	   (paragraphs (json-string->scm (get-string-all p)))
	   (dummy (close-port p))
	   (_ (set! counter 0))
	   (_ (while (> 5 counter )
	    	(begin
		  (pretty-print (assoc-ref paragraphs "100"))
		  (set! counter (+ counter 1)))))
	

	   )
      (begin
	#t
       	(pretty-print (car results-sorted))
;;	(save-list-to-json "ppan-sorted-embeds" results-sorted *top-dir*)
;;	(pretty-print (vector-length query-embedding))
;;	(pretty-print (vector-length (assoc-ref a "100")))
;;	(pretty-print (vector-length (assoc-ref a "0")))
	)
      ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/mbc/projects/gollama")'
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/ubuntu/gollama")'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main args)
  (let* ((start-time (current-time time-monotonic))
	 
	 (_  (pretty-print (string-append "args: " args)))
	 (_ (set-envs (get-envs  args)))
	 (_  (pretty-print (string-append "in main: " *chat-uri*)))
	 (_  (pretty-print (string-append "in main: " *model*)))
	;; (_ (pretty-print (get-embedding *embeddings-uri* *model* "sometext" )))
	;; (pretty-print (ingest-doc "/home/mbc/projects/gollama/text/minppan.txt" "1234" *model* *embeddings-uri*))
	 ;;  (ems (get-embeddings uri "mistral" chunk-lst '()))
	 ;;(paragraphs (collect-paragraphs "/home/mbc/projects/gollama/text/minppan.txt"))
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (time-second (time-difference stop-time start-time))))
	 )
    (begin
      (get-sorted-scores "ppan-embeddings-2024120403091733324998.json")
;;      (pretty-print (cosine-sim #(1 2 3 4 5) #(5 6 7)))
  ;;    (pretty-print (acons 1 "hello" '()) )
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use.")))))


