(define-module (gollama db) 
	     #:use-module (srfi srfi-19)   ;; date time
	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     #:use-module (srfi srfi-9)  ;;records
	     #:use-module (ice-9 rdelim)
	     #:use-module (ice-9 popen)
	     #:use-module (ice-9 regex) ;;list-matches
	     #:use-module (ice-9 receive)
	     #:use-module (ice-9 format)
	     #:use-module (ice-9 string-fun)  ;;string-replace-substring
	     #:use-module (ice-9 pretty-print)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (ice-9 ftw) ;; file tree walk
	     #:use-module (ice-9 readline) ;;must sudo apt-get install libreadline-dev; guix package -i guile-readline
	     #:use-module (json)
	     #:use-module (gollama utilities)
	     #:export (make-doc-list-element
		       save-list-to-json
		       get-json-from-file
		       add-doc-entry
		       )	     
	     )


(define (make-doc-list-element file model algo)
  ;;expecting text file with .txt extension
  ;;file is the full path - must ingest document for md5
  ;;algo: cosine-similarity:cosine-sim 
  (let* (;;(id (get-file-md5 file))
	 (title  (basename file ".txt"))
	 (id (substring (get-sha256 (string-append title model algo)) 0 12))
	  )
   `(("id" . ,id)("doc" . ,file)("title" . ,title)("model" . ,model)("algorithm" . ,algo)("date" . ,(date->string  (current-date) "~y~m~d~I~M~S"))("embeddings" . ,(string-append id "-embe.json" ))("paragraphs" . ,(string-append id "-para.json")))))



(define (save-list-to-json file-name lst top-dir )
  ;;json for db
  (let* (;;(vec (list->vector lst))
;;	 (content (scm->json-string `(("books" .  ,vec))))
	 (content (scm->json-string lst))
	 
	 (a (string-append top-dir "/db/" file-name ".json"))
	 (out-port (open-file a "a")))
    (begin
	 (put-string out-port content)
	 (force-output out-port)
	 (close-port out-port))))

(define (get-json-from-file file)
  ;; returns the vector converted to list
  (let* (
	 (pretty-print (string-append "opening file name: " file))
	 (p  (open-input-file file))
	 (data (json->scm p)))
     (vector->list data)))

;;working here!!!
(define (add-doc-entry doclst top-dir)
  ;;new element is '(old-fname new-fname '(list of attributes))
  ;;use (caddr to get it)
  (let* ( (_ (pretty-print (string-append top-dir "/db/db.json")))
	;; (olddocs (vector->list (assoc-ref (get-json-from-file (string-append top-dir "/db/db.json") ) "docs")))
	  (olddocs  (vector->list (assoc-ref (car (get-json-from-file (string-append top-dir "/db/db.json") )) "docs")))
	 (_ (pretty-print  olddocs))
	 (newdocs (cons doclst olddocs))
	 )
  ;;  #f
    (pretty-print newdocs)
   ;; (save-list-to-json "db" newdocs top-dir)
    ))
  
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (define (get-all-books);;as list
;;   (let* ((books-file-name  (get-books-json-fn))
;; ;;	 (_ (pretty-print (string-append "target: " target)))
;; ;;	 (_ (pretty-print (string-append "books-file-name: " books-json)))
	 
;; 	 (p  (open-input-file books-file-name))
;; 	 (all-books (json->scm p))
;; 	 (book-vec (assoc-ref all-books "books"))
;; 	 ;;(tag-vec (assoc-ref all-tags "tags"))
;; 	 )
;;     (vector->list book-vec)))



(define (cons-books-to-lib new old)
  ;;new element is '(old-fname new-fname '(list of attributes))
  ;;use (caddr to get it)
  (if (null? (cdr new))
      (begin
	(set! old (cons  (caddar new) old))
	old)
      (begin
	(set! old (cons  (caddar new) old))
	(cons-books-to-lib (cdr new) old))))



(define (make-json-for-gs lst top-dir)
  ;;json for import in graph-store
  (let* ((vec (list->vector lst))
	 (content (scm->json-string `(("books" .  ,vec))))
	 (pref (date->string  (current-date) "~Y~m~d~I~M"))
	 (gs-filename (string-append top-dir "lib/" pref "-forgs.json"))
	 (out-port (open-output-file gs-filename))
	 (dummy (put-string out-port content)))
    (force-output out-port)))


(define (get-all-books-as-string lst out)
  (if (null? (cdr lst))
      (begin
	(set! out (string-append (car lst) "\n" out))
	out)
      (begin
	(set! out (string-append (car lst) "\n" out))
	(get-all-books-as-string (cdr lst) out))))



(define (recurse-get-books-with-title titl lst results counter)
  ;;results is a list of books for given title
  (if (null? (cdr lst))
      (if (string-contains-ci (assoc-ref (car lst) "title") titl)
	  (begin
	    (set! results (cons (acons "counter" (number->string counter) (car lst)) results))
	    results)
	  results)
      (if (string-contains-ci (assoc-ref (car lst) "title") titl)
	  (begin
	    (set! results (cons (acons "counter" (number->string counter) (car lst)) results))	    
	    (set! counter (+ counter 1))
	    (recurse-get-books-with-title titl (cdr lst) results counter))
	  (recurse-get-books-with-title titl (cdr lst) results counter))
      ))

(define (get-books-with-title tits)
  (let* ((all-books  (get-all-books)))
    (recurse-get-books-with-title tits all-books '() 1)))



(define (get-books-for-author aut)
  (let* ((all-books  (get-all-books)))
    (recurse-get-books-for-author aut all-books '() 1)))

;;(vector-index            #("Dodo Doodoo" "Plain Jane" "Joer Blow"))

;; (define (recurse-get-books-for-author auth lst results)
;;   ;;results is a list of books for given author
;;   (if (null? (cdr lst))
;;       (if (member auth (vector->list (assoc-ref (car lst) "author")))
;; 	  (cons (car lst) results) results)
;;       (if (member auth (vector->list (assoc-ref (car lst) "author")))     
;; 	  (cons (car lst) results)
;; 	  (recurse-get-books-for-author auth (cdr lst) results))))


(define (recurse-get-books-for-author auth lst results counter)
  ;;results is a list of books for given author
  (if (null? (cdr lst))
      (if (string-contains (apply string-append (vector->list (assoc-ref (car lst) "author"))) auth)
	  (begin
	    (set! results (cons (acons "counter" (number->string counter) (car lst)) results))
	    results)	  
	  results)
      (if (string-contains (apply string-append (vector->list (assoc-ref (car lst) "author"))) auth)
	  (begin
	    (set! results (cons (acons "counter" (number->string counter) (car lst)) results))
	     (set! counter (+ counter 1))
	    (recurse-get-books-for-author auth (cdr lst) results counter))
	  (recurse-get-books-for-author auth (cdr lst) results counter)
	  )))



;;(vector-index            #("Dodo Doodoo" "Plain Jane" "Joer Blow"))

(define (recurse-get-books-with-tag tag lst results counter)
  ;;results is a list of books for given tag
  ;;store the counter as a string
  (if (null? (cdr lst))
      (if (member tag (vector->list (assoc-ref (car lst) "tags")))
	  (begin
	    (set! results (cons (acons "counter" (number->string counter) (car lst)) results))
	    results)
	  results)
	  (if (member tag (vector->list (assoc-ref (car lst) "tags")))
	  (begin    
	    (set! results (cons (acons "counter" (number->string counter) (car lst)) results))	    
	    (set! counter (+ counter 1))
	    (recurse-get-books-with-tag tag (cdr lst) results counter))
	  (recurse-get-books-with-tag tag (cdr lst) results counter)
	  )))


(define (get-books-with-tag tag)
  (let* ((all-books  (get-all-books)))
    (recurse-get-books-with-tag tag all-books '() 1)))


(define (recurse-get-book-with-isbn isbn lst results)
  ;;results is a list of books for given isbn
  (if (null? (cdr lst))
      (if (string=? (assoc-ref (car lst) "isbn") isbn) (cons (car lst) results) results)       
      (if (string=? (assoc-ref (car lst) "isbn") isbn)
	  (cons (car lst) results)
	  (recurse-get-book-with-isbn isbn (cdr lst) results))
      ))

(define (get-book-with-isbn isbn top-dir)
  (let* ((all-books  (get-all-books)))
    (recurse-get-book-with-isbn isbn all-books '())))



(define (recurse-get-book-with-id id lst results)
  ;;results is a list of books for given id
  (if (null? (cdr lst))
      (if (string=? (assoc-ref (car lst) "id") id) (cons (car lst) results) results)       
      (if (string=? (assoc-ref (car lst) "id") id)
	  (cons (car lst) results)
	  (recurse-get-book-with-id id (cdr lst) results))
      ))

(define (get-book-with-id id )
  (let* ((all-books  (get-all-books)))
    (recurse-get-book-with-id id all-books '())))



(define (assign-tags-to-book id tta)
  ;;id: the hash of the book
  ;;tta: tags to asign as a list
  (let* (
	 (orig (car (get-book-with-id id)))
	 (orig-tags (vector->list (assoc-ref orig "tags")))
	 (new-book orig)
	 (new-tags (list->vector(append orig-tags tta)))
	 (new-book (assoc-set! new-book "tags" new-tags)))
    new-book
    ))

(define (add-tags-to-book tags book)
  ;;adds tags to a book
  ;;book is the book item as an element
  (let* ((old-tags (vector->list (assoc-ref book "tags")))
	 (new-tags (list->vector (append tags old-tags)))
	 (new-book (assoc-set! book "tags" new-tags)))
new-book))



(define (substitute-new-for-old-book new old)
  ;;updates the main library list with edited book
  (let* ((all-books (get-all-books))
	 (mod-books (cons new (delete old all-books))) )
    mod-books))

