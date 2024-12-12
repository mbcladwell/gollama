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
 #:use-module (gollama db)
 #:use-module (rnrs sorting) ;;list-sort

 #:export (get-message
	   send-chat
	   cosine-sim
	   collect-paragraphs
	   get-embedding
	   ;;	   recurse-get-embedding
	   ingest-doc
	   get-sorted-scores
	   get-first-n-list
	   get-top-hits
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

(define (cosine-sim a b)
;;cosine similariy
  ;; https://datastax.medium.com/how-to-implement-cosine-similarity-in-python-505e8ec1d823
  ;; x and y must be lists of similar lengths
  (let* ((x (vector->list a))
	 (y (vector->list b))
	 (dot-product (map * x y))
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

(define (count-tokens lst)
   (fold (lambda (x prev)
	   (let*((a (length (string-split x #\space)))
		 )	 	  
	  (cons a prev)))
        '() lst))


(define (get-min-tokens lst holder)
  ;;return a minimum number of tokens a paragraph should have
  ;;(get-min-tokens counts '())
  ;;counts is from count-tokens
  (if (null? (cdr lst))
      (let* ((_  (if (> (car lst) 20)(set! holder (cons (car lst) holder))))
	     (index (round (/ (length holder) 2))))	     
	(vector-ref (list->vector holder) index))
      (begin
	(if (> (car lst) 20)(set! holder (cons (car lst) holder)))
	(get-min-tokens (cdr lst) holder))))


(define (normalize-para-lengths lst min-length holder out)
  ;;combine neighboring paragraphs so that they have a minimum number of tokens
  ;;lst: list of paragraphs
  (if (null? (cdr lst))
      (begin
	(if (> (length (string-split (string-concatenate holder) #\space)) 1)
	    (begin
	       (set! out (cons (string-concatenate holder) out))
	       ))
	(set! out (cons (car lst) out))
	 out)
      (begin
	(if (> (length (string-split (car lst) #\space)) min-length)
	    (begin
	      (set! out (cons (car lst) out))
	      (normalize-para-lengths (cdr lst) min-length holder out)
	      )
	    (begin
	      (set! holder (cons (car lst) holder))
	      (set! holder (cons " " holder))
	      (if (> (length (string-split (string-concatenate holder) #\space)) min-length)
		  (begin
		   ;; (pretty-print holder)
		    (set! out (cons (string-concatenate holder) out))
		    (set! holder '())
		    ))
	      (normalize-para-lengths (cdr lst) min-length holder out)	      
	      )))
      ))


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

(define (get-embedding uri model chunk)
  (let* ((a (receive (response body)
	       (http-request uri
			     #:method 'POST
			     #:body (scm->json-string `(("model" . ,model)("input" . ,chunk)))
			     #:streaming? #f
			     #:verify-certificate? #f)
	      (utf8->string body)))	   
	 (b (assoc-ref (json-string->scm a)  "embeddings")))
    (vector-ref b 0)))



(define (recurse-get-embedding uri model lst out)
  ;;lst is the input list of text chunks
  ;;out is the output list of embeddings
  ;; (get-embeddings "mistral" chunk-lst '())
  (if (null? (cdr lst))
      (begin 
	(set! out (cons (get-embedding uri model (car lst)) out))
	out)
      (begin
	(set! out (cons (get-embedding uri model (car lst)) out))
	(get-embeddings uri model (cdr lst) out))))



(define (recurse-process-para id para counter plst elst model uri top-dir)
  ;;para: the list of normalized paragraphs
  ;;plst alst of paragraphs
  ;;elst alst of embeddings
  ;;(recurse-process-para "jdk8suyar9" lst 0 '() '() embeddings-model uri)
  (if (null? (cdr para))
      (let* ((text (car para))
	     (embedding (get-embedding uri model text)))
	(begin
	  (set! plst (acons counter text plst))
	  (set! elst (acons counter embedding elst))
	;;  (list plst elst)
	  (save-list-to-json (string-append id "-embe.json") elst top-dir)
	  (save-list-to-json (string-append id "-ipar.json") plst top-dir)
	  ))
      (let* ((text (car para))
	     (embedding (get-embedding uri model text)))
	(begin
	  (set! plst (acons counter text plst))
	  (set! elst (acons counter embedding elst))
	  (recurse-process-para id (cdr para) (+ counter 1) plst elst model uri top-dir)
	  ))))

;; (define (recurse-process-para para counter plst elst model embeddings-uri)
;;   ;;para: the list of normalized paragraphs
;;   ;;plst alst of paragraphs
;;   ;;elst alst of embeddings
;;   ;;(recurse-process-para lst 0 '() '() model embeddings-uri)
;;   (if (null? (cdr para))
;;       (let* ((text (car para))
;; 	     (embedding (get-embedding embeddings-uri model text)))
;; 	(begin
;; 	  (set! plst (acons counter text plst))
;; 	  (set! elst (acons counter embedding elst))
;; 	  (list plst elst)
;; 	  ))
;;       (let* ((text (car para))
;; 	     (embedding (get-embedding embeddings-uri model text)))
;; 	(begin
;; 	  (set! plst (acons counter text plst))
;; 	  (set! elst (acons counter embedding elst))
;; 	  (recurse-process-para (cdr para) (+ counter 1) plst elst model embeddings-uri)
;; 	  ))))

  

(define (ingest-doc file embeddings-model embeddings-uri top-dir algorithm)
  ;;create the json index element in db.json
  
  (let* (;;(doc-name (basename file ".txt"))
	 ;;makes doc-lst; adds to db; backs up old db;
	 (doc-lst (make-doc-list-element file embeddings-model algorithm))
	 (_ (add-doc-entry doc-lst top-dir))
	 (_ (pretty-print (string-append "id: " (assoc-ref doc-lst "id"))))
	 (_ (pretty-print (string-append "document: " (assoc-ref doc-lst "doc"))))
	 (_ (pretty-print (string-append "title: " (assoc-ref doc-lst "title"))))
	 (_ (pretty-print (string-append "model: " (assoc-ref doc-lst "model"))))
	 (_ (pretty-print (string-append "algorithm: " (assoc-ref doc-lst "algorithm"))))
	 (_ (pretty-print (string-append "date: " (assoc-ref doc-lst "date"))))
	 (paragraphs (collect-paragraphs file))
	 (_ (pretty-print (string-append "Paragraph count original doc: " (number->string (length paragraphs)))))
	 ;;count token per paragraph and sort; determine min number of tokens
	 ;;by removing counts <20 then taking the middle value of remaining
	 (num-tokens  (list-sort > (count-tokens paragraphs)))
	 (min-tokens (get-min-tokens num-tokens '()))
	 (_ (pretty-print (string-append "Minimum tokens per paragraph: " (number->string min-tokens))))
	 ;;normalize the paragraph number i.e. make sure all paragraphs have min-tokens tokens
	 (norm-para (normalize-para-lengths paragraphs min-tokens '() '()))
	 (_ (pretty-print (string-append "Paragraph count norm doc: " (number->string (length norm-para)))))
	 
;;	 (results (recurse-process-para norm-para 0 '() '() embeddings-model embeddings-uri))	  
;;	 (para-alst (car results))
	 ;;	 (embed-alst (cadr results))
	 (file-name (string-append top-dir "/db/" (assoc-ref doc-lst  "id") "-npar.json"))
	   )
     (begin
       ;;   (save-list-to-json (assoc-ref doc-lst "embeddings") embed-alst top-dir)
       
     (send-json-to-file "norm-para" norm-para file-name))
   ))


(define (sort-embeddings x y)
  ;;https://www.gnu.org/software/guile/manual/html_node/rnrs-sorting.html
  (> (assoc-ref x "embedding")(assoc-ref y "embedding")))


(define (get-sorted-scores query file embeddings-uri model top-dir)
  ;;get sorted scores for a query compared to a corpus
  ;;file of embeddings for validated document
  ;;query: text to be compared
  (define counter 0)
  (define scores '())
    (let* (
	   (p  (open-input-file (string-append top-dir "/db/" file)))
	   (haystack (json-string->scm (get-string-all p)))
	   (dummy (close-port p))
	   (haystack-length (length haystack))
	   (needle (get-embedding embeddings-uri model query))
	;;   (needle (assoc-ref haystack "100"))
	  ;; (score (cosine-sim needle (assoc-ref haystack counter)))
	   (_ (while (> haystack-length counter)
	    	(begin
		  (set! scores (cons `(("id" . ,(number->string counter))("embedding" . ,(cosine-sim needle (assoc-ref haystack (number->string counter))))) scores))
	    	  (set! counter (+ 1 counter))))))
      (list-sort sort-embeddings scores)))

(define (get-first-n-list lst n counter results)
  ;;(get-first-n-list lst 5 0 '())
  (if (or (null? (cdr lst)) (= counter (- n 1)))
      (begin
	(set! results (cons (car lst) results))	    
	results)
      (begin
	(set! results (cons (car lst) results))	    
	(set! counter (+ counter 1))
	(get-first-n-list (cdr lst) n counter results))))

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

(define (get-top-hits needle haystack N paragraphs embeddings-uri model top-dir)
  ;;N number of hits desired
  ;;haystack: all embeddings for text
  ;;needle: query to be compared to haystack
  ;;paragraphs: file name of the list of paragraphs of the text;
 ;; will return highest scoring paragraphs concatenated into a single paragraph
    (let* (
	   (sorted-scores (get-sorted-scores needle haystack embeddings-uri model top-dir))
	   (top-5-scores (get-first-n-list sorted-scores 5 0 '()))
	 ;;  (_ (pretty-print (string-append "paras: " top-5-scores)))
	   (p  (open-input-file (string-append top-dir "/db/" paragraphs)))
	   (content (json-string->scm (get-string-all p)))
	   (_ (close-port p))	   
	   (paras (string-concatenate (recurse-paragraphs-for-ids top-5-scores content '())))
	   )
      (pretty-print paras)))
