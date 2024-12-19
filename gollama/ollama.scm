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
 #:use-module (gollama textman)
 #:use-module (gollama db)
 #:use-module (rnrs sorting) ;;list-sort

 #:export (get-message
	   send-chat
	   cosine-sim
	   get-embedding
	   recurse-process-para
	   get-chat-response
	   ingest-doc
	   get-sorted-scores
	   get-first-n-list
	   get-top-hits
	   normalize-para-lengths
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


(define (get-chat-response uri data)
  (let* ((a (receive (response body)
		(http-request uri
			      #:method 'POST
			      #:body  (scm->json-string data)
			      #:streaming? #f
			      #:verify-certificate? #f)
	      (utf8->string body)))
	 (b (assoc-ref (json-string->scm a) "choices")))
	(assoc-ref (assoc-ref (vector-ref b 0) "message") "content")))


;;	 (b (assoc-ref (json-string->scm a)  "embeddings")))
  ;;  (vector-ref b 0)))


  



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



;; (define (recurse-process-para id para counter plst elst model uri top-dir)
;;   ;;para: the list of normalized paragraphs
;;   ;;plst alst of paragraphs
;;   ;;elst alst of embeddings
;;   ;;(recurse-process-para "jdk8suyar9" lst 0 '() '() embeddings-model uri)
;;   (if (null? (cdr para))
;;       (let* ((text (car para))
;; 	     (embedding (get-embedding uri model text)))
;; 	(begin
;; 	  (set! plst (acons counter text plst))
;; 	  (set! elst (acons counter embedding elst))
;; 	;;  (list plst elst)
;; 	  (save-to-json  elst (string-append top-dir "/db/" id "/embedded-paragraphs.json") #t)
;; 	  (save-to-json  plst (string-append top-dir "/db/" id "/indexed-paragraphs.json") #t)
;; 	  ))
;;       (let* ((text (car para))
;; 	     (embedding (get-embedding uri model text)))
;; 	(begin
;; 	  (set! plst (acons counter text plst))
;; 	  (set! elst (acons counter embedding elst))
;; 	  (recurse-process-para id (cdr para) (+ counter 1) plst elst model uri top-dir)
;; 	  ))))


(define (recurse-process-para id para elst model uri top-dir)
  ;;para: the list of normalized paragraphs
  ;;plst alst of paragraphs
  ;;elst alst of embeddings
  ;;(recurse-process-para "jdk8suyar9" lst 0 '() '() embeddings-model uri)
  (if (null? (cdr para))
      (let* ((index (caar para))
	     (text (cdar para))
	     (embedding (get-embedding uri model text)))
	(begin
	  (set! elst (acons index embedding elst))
	  ;; (save-to-json  elst (string-append top-dir "/db/" id "/embedded-paragraphs.json") #t)
	  elst
	  ))
      (let* ((index (caar para))
	     (text (cdadr para))
	     (embedding (get-embedding uri model text)))
	(begin
	  (set! elst (acons index embedding elst))
	  (recurse-process-para id (cdr para) elst model uri top-dir)
	  ))))

  

(define (ingest-doc file chat-model embeddings-model embeddings-uri top-dir algorithm)
  ;;create the json index element in db.json
  ;;file is file name only, must be in text directory
  (let* (;;(doc-name (basename file ".txt"))
	 ;;makes doc-lst; adds to db; backs up old db;
	 (doc-lst (make-doc-list-element file chat-model embeddings-model algorithm))
	 (_ (add-doc-entry doc-lst top-dir))
	 (id (assoc-ref doc-lst "id"))
	 (_ (pretty-print (string-append "id: " id)))
	 (_ (mkdir (string-append top-dir "/db/" id)))
	 (_ (pretty-print (string-append "document: " (assoc-ref doc-lst "doc"))))
	 (_ (pretty-print (string-append "title: " (assoc-ref doc-lst "title"))))
	 (_ (pretty-print (string-append "chat-model: " (assoc-ref doc-lst "chat-model"))))
	 (_ (pretty-print (string-append "embeddings-model: " (assoc-ref doc-lst "embeddings-model"))))
	 (_ (pretty-print (string-append "algorithm: " (assoc-ref doc-lst "algorithm"))))
	 (_ (pretty-print (string-append "date: " (assoc-ref doc-lst "date"))))
	 (source-file (string-append top-dir "/text/" file))
	 (dest-file (string-append top-dir "/db/" id "/" file))
	 (paragraphs (collect-paragraphs source-file))		     
	 (_ (pretty-print (string-append "Paragraph count original doc: " (number->string (length paragraphs)))))
	 ;;count token per paragraph and sort; determine min number of tokens
	 ;;by removing counts <20 then taking the middle value of remaining
	 (num-tokens  (list-sort > (count-tokens paragraphs)))
	 (min-tokens (get-min-tokens num-tokens '()))
	 (_ (pretty-print (string-append "Minimum tokens per paragraph: " (number->string min-tokens))))
	 ;;normalize the paragraph number i.e. make sure all paragraphs have min-tokens tokens
	 (norm-para (normalize-para-lengths paragraphs min-tokens '() '()))
	 (_ (pretty-print (string-append "Paragraph count norm doc: " (number->string (length norm-para)))))
	 (indexed-paragraphs (provide-indices norm-para 0))	 
	 (file-name (string-append top-dir "/db/" (assoc-ref doc-lst  "id") "/indexed-paragraphs.json"))
	   )
     (begin
       ;;   (save-list-to-json (assoc-ref doc-lst "embeddings") embed-alst top-dir)       
       (save-to-json indexed-paragraphs file-name #t)
       (copy-file source-file dest-file)
       (delete-file source-file)
       (process-prompt-files id top-dir)
       )))


(define (sort-embeddings x y)
  ;;https://www.gnu.org/software/guile/manual/html_node/rnrs-sorting.html
  (> (assoc-ref x "embedding")(assoc-ref y "embedding")))




(define (get-sorted-scores needle haystack)
  ;;get sorted scores for a query compared to a corpus
  ;; embeddings for validated document
  ;;query: embedding of text to be compared
  (define counter 1)
  (define scores '())
    (let* (;;(_ (pretty-print "in get-sorted-scores"))
	   (haystack-length (length haystack))
	   ;;   (_ (pretty-print haystack-length))
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


(define (get-top-hits needle haystack n id top-dir)
  ;;n number of hits desired
  ;;haystack: all embeddings for text
  ;;needle: query to be compared to haystack
  ;;id: directory id; paragraphs are in indexed-paragraphs.json
 ;; will return highest scoring paragraphs concatenated into a single paragraph
    (let* (
	   (sorted-scores (get-sorted-scores needle haystack))
	   (top-5-scores (get-first-n-list sorted-scores n 0 '()))
	   (content  (get-list-from-json-file (string-append top-dir "/db/" id "/indexed-paragraphs.json") #t))
	   (paras (string-concatenate (recurse-paragraphs-for-ids top-5-scores content '())))
	   )
       paras))
