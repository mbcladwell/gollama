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
		       send-json-to-file
		       save-to-json
		       get-list-from-json-file
		       add-doc-entry
		       )	     
	     )


(define* (send-json-to-file resource data file-name)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;;data must be a list
  (let* (
;;	 (stow (scm->json-string (acons resource (list->vector data) '())))
	 (stow (scm->json-string  (list->vector data) ))
	 (p  (open-output-file file-name))
	 (_ (put-string p stow)))
    (force-output p)))

(define (save-to-json data file-name)
  ;;may need to (cons data '()) outside function 
  (let* (
	 (stow (scm->json-string  (list->vector data) ))
	 (p  (open-output-file file-name))
	 (_ (put-string p stow)))
    (force-output p)))

  

(define (make-doc-list-element file model algo)
  ;;expecting text file with .txt extension
  ;;file is the full path - must ingest document for md5
  ;;algo: cosine-similarity:cosine-sim 
  (let* (;;(id (get-file-md5 file))
	 (title  (basename file ".txt"))
	 (id (substring (get-sha256 (string-append title model algo)) 0 12))
	  )
    `(("id" . ,id)("doc" . ,file)("title" . ,title)("model" . ,model)("algorithm" . ,algo)
      ("date" . ,(date->string  (current-date) "~y~m~d~I~M~S")))))


(define (make-backup-file-name file-name  top-dir)
  (string-append top-dir "/backup/" (basename file-name ".json") (date->string  (current-date) "-~Y~m~d~H~M~S") ".json"))

 
(define (get-list-from-json-file file )
  ;; returns the vector converted to list
  (let* ((p  (open-input-file file))
	 (data  (json->scm p))
	;; (_ (pretty-print "data in get-json:"))
	;; (_ (pretty-print data))
	 )
      (car (vector->list data))))


(define (add-doc-entry doclst top-dir)
  (let* (;; (_ (pretty-print (string-append top-dir "/db/db.json")))
	 (db-fn (string-append top-dir "/db/db.json"))
	 (bak-fn (make-backup-file-name "db.json" top-dir))
	 (_ (copy-file db-fn bak-fn))
	 (olddocs   (get-list-from-json-file db-fn))
	 (newdocs (cons doclst olddocs))
	 )
    (begin
      (delete-file db-fn)
      (save-to-json newdocs db-fn)
    )))
  
 
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

