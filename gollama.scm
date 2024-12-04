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




;; (define (recurse-get-embedding model lst out counter embeddings-file chunks-file)
;;   ;;lst is the input list of text chunks
;;   ;;out is the output list of embeddings
;;   ;; (get-embeddings "mistral" chunk-lst '() 0)
;;   (if (null? (cdr lst))
;;       (begin 
;; 	(set! out (cons (get-embedding model (car lst)) out))
;; 	out)
;;       (begin
;; 	(set! out (cons (get-embedding model (car lst)) out))
;; 	(get-embeddings model (cdr lst) out))))



(define (recurse-process-para para counter plst elst)
  ;;para: the list of paragraphs
  ;;plst alst of paragraphs
  ;;elst alst of embeddings
  ;;(recurse-process-para lst 0 '() '())
  (if (null? (cdr para))
      (let* ((text (car para))
	     (embedding (get-embedding "mistral" text)))
	(begin
	  (set! plst (acons counter text plst))
	  (set! elst (acons counter embedding elst))
	  (list plst elst)
	  ))
      (let* ((text (car para))
	     (embedding (get-embedding "mistral" text)))
	(begin
	  (set! plst (acons counter text plst))
	  (set! elst (acons counter embedding elst))
	  (recurse-process-para (cdr para) (+ counter 1) plst elst)
	  ))))
  
  

 (define (ingest-doc doc id)
   (let* ((doc-name (basename doc ".txt"))
	  (doc-lst (make-doc-list-element doc-name id))
	 ;; (dot (string-rindex str #\.)) ;;reverse search
	  ;; (pref (substring str 0  dot ))
	   (paragraphs (collect-paragraphs doc))
	   (results (recurse-process-para paragraphs 0 '() '()))	  
	   (para-alst (car results))
	   (embed-alst (cadr results))
	   )
     (begin
     (save-list-to-json (string-append doc-name "-embeddings") embed-alst *working-dir*)
     (save-list-to-json (string-append doc-name "-paragraphs") para-alst *working-dir*))
   ))

;;https://www.gnu.org/software/guile/manual/html_node/rnrs-sorting.html

;; (define my-list '(
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 1))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 5))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 3))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 6))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 2))
;; 		  (("a" . 1)("b" . 1)("c" . 1)("d" . 4))
;; 		  ))

(define (sort-embeddings x y)
  (> (assoc-ref x "embedding")(assoc-ref y "embedding")))

  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/mbc/projects/gollama")'
;;guix shell -m manifest.scm -- guile -l "gollama.scm" -c '(main "/home/ubuntu/gollama")'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main args)
  (let* ((start-time (current-time time-monotonic))
	 
	 (_  (pretty-print (string-append "args: " args)))
	 (_ (set-envs (get-envs  args)))
	 (_  (pretty-print (string-append "in main: " *ollama-uri*)))
	 ;;  (ems (get-embeddings "mistral" chunk-lst '()))
;;	 (a (get-with-http "Why is the sky blue?" "mistral"))
	 ;;(paragraphs (collect-paragraphs "/home/mbc/projects/gollama/text/ppan.txt"))
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (time-second (time-difference stop-time start-time))))
	 )
    (begin
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use."))
      ;;(pretty-print ems)
      (pretty-print (ingest-doc "/home/mbc/projects/gollama/text/minppan.txt" "1234"))
;;      (pretty-print (cosine-sim #(1 2 3 4 5) #(5 6 7)))
  ;;    (pretty-print (acons 1 "hello" '()) )
       (pretty-print (get-embedding "mistral" "sometext" ))
      
      (pretty-print a)
      )))


