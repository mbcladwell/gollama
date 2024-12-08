(define-module (gollama menus) 
	     #:use-module (srfi srfi-19)   ;; date time
	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     #:use-module (ice-9 rdelim)
	     #:use-module (ice-9 popen)
	     #:use-module (ice-9 regex) ;;list-matches
	     #:use-module (ice-9 format)
	     #:use-module (ice-9 string-fun)  ;;string-replace-substring
	     #:use-module (ice-9 pretty-print)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (ice-9 ftw) ;; file tree walk
	     #:use-module (ice-9 readline) ;;must sudo apt-get install libreadline-dev; guix package -i guile-readline
	     #:use-module (json)
	     #:export (display-logo)
	     #:export (display-main-menu)
	     #:export (display-results)
	     #:export (display-results-header)
	     #:export (add-tag-menu-item)
	     #:export (add-suffix-menu-item)
	     #:export (display-query-submenu)
	     #:export (display-init-menu)
;	     #:export ()
;	     #:export ()
	     
	     )


 (define (display-logo top-dir)
   ;;https://patorjk.com/software/taag/#p=display&f=Big&t=Book%20Munger
   (begin
     ;;     (system "printf \"\\033c\"")
(display "   _____        _ _                      \n")
(display "  / ____|      | | |      \n")                
(display "  | |  __  ___ | | | __ _ _ __ ___   __ _ \n")
(display "  | | |_ |/ _ \\| | |/ _` | '_ ` _ \\ / _` |\n")
(display "  | |__| | (_) | | | (_| | | | | | | (_| |\n")
(display "  \\______|\\___/|_|_|\\__,_|_| |_| |_|\\__,_|\n")
     (display (string-append "top-dir: " top-dir "\n\n"))
     ))

(define (display-main-menu)
  (begin
    (display-logo)
  
    (display "1 Ingest document\n")
    (display "2 Process deposit files\n")
    (display "3 Add a tag to controlled list\n")
    (display "4 Add suffix\n\n")
    (display "Ctrl-z to exit\n\n")
  ))

(define (display-init-menu)
  (begin
    (display-logo)
    (display "Storage method\n")
    (display "==============\n")
    (display "1 File\n")
    (display "2 Oracle S3\n")
    (display "3 AWS S3\n")
    (display "4 Minio local\n")
    (display "5 Minio remote\n\n")
    
    (display "Ctrl-z to exit\n\n")
  ))


(define (add-tag-menu-item)
  (let* ((_ (pretty-print (get-all-tags)))	 
	 (a-tag (readline "\nEnter tag to add to controlled list: ")))
    (add-tag-to-controlled-list a-tag)))

(define (add-suffix-menu-item)
  (let* ((result (readline "Enter suffix to add to controlled list: ")))
    (add-suffix result)))

(define (display-query-submenu)
  (let* (
	 (dummy (display-logo))
	 (dummy (display "1 Query by keyword\n"))
	 (dummy (display "2 Query by title\n"))
	 (dummy (display "3 Query by author\n\n"))
	 (dummy (display "Ctrl-z to exit\n\n"))
	 (selection (readline "Selection: "))
	 )
     (cond ((string= selection "1") (query-by-tag))
 	 ((string= selection "2") (query-by-title))
	 ((string= selection "3") (query-by-author))
    )))


(define (find-element-with-counter lst counter)
  (if (null? (cdr lst))      
      (if (string= (assoc-ref (car lst) "counter") counter)(car lst) #f)
      (begin
	(if (string= (assoc-ref (car lst) "counter") counter)
	    (car lst)
	    (find-element-with-counter (cdr lst) counter))
      )))






(define (query-by-tag)
  ;;query-for-tags is different - it fills out tag shortcuts
  ;;this is a menu item
  (let* ((dummy (display-logo))
;;	 (dummy (display (get-all-tags-as-string db-dir tags-file-name)))
	 (dummy (display-tag-menu))
	 (all-tags (get-all-tags))
	 (in (readline "Select tag: "))
	 (the-tag (recurse-desired-tag in all-tags))
	 (lst (get-books-with-tag the-tag))
	 )
    (if (= (length lst) 0)
	(display "Match not found!\n\n")
	(let* ((dummy (display-results-header))
	       (dummy (display-results (reverse lst)))
	       
	       (dummy (display "\nActivities: t - add tag"))
	       (dummy (display "\n            w - withdraw"))
	       (dummy (display "\nIndicate as a doublet e.g  '1 t' to add tags to book 1"))	       
	       (what-do  (readline "\nAction: [1 t]: "))
	       (a (string-split what-do #\space))
               (abook  (find-element-with-counter lst (car a)))
	       (id (assoc-ref abook "id"))
	       (action (cadr a))	       
	       )	  
	  (cond ((string= action "t")
		 (let* ((dummy (display-tag-menu))
			(the-tags (query-for-tags))
			(new-book (assign-tags-to-book id the-tags))
			(orig (car (get-book-with-id id)))
			(mod-all-books (substitute-new-for-old-book new-book orig))
			)
		   		   (begin
		     (backup-json "books")
		     (send-json-to "books" mod-all-books))))		 
 		 ((string= action "w") 
		  (let* (
	      		 (orig (car (get-book-with-id id)))
		;	 (dummy (pretty-print orig))
			)
		  (move-to-withdraw orig))			  
		 )
		)))))


(define (query-by-author)
  ;;query-for-tags is different - it fills out tag shortcuts
  ;;this is a menu item
  (let* ((dummy (display-logo))
	 (in (readline "Select author: "))
	 (lst (get-books-for-author in))
	 )
    (if (= (length lst) 0)
	(display "Match not found!\n\n")
	(let* ((dummy (display-results-header))
	       (dummy (display-results (reverse lst)))
	       
	       (dummy (display "\nActivities: t - add tag"))
	       (dummy (display "\n            w - withdraw"))
	       (dummy (display "\nIndicate as a doublet e.g  '1 t' to add tags to book 1"))	       
	       (what-do  (readline "\nAction: [1 t]: "))
	       (a (string-split what-do #\space))
               (abook  (find-element-with-counter lst (car a)))
	       (id (assoc-ref abook "id"))
	       (action (cadr a))	       
	       )	  
	  (cond ((string= action "t")
		 (let* ((dummy (display-tag-menu))
			(the-tags (query-for-tags))
			(new-book (assign-tags-to-book id the-tags))
			(orig (car (get-book-with-id id)))
			(mod-all-books (substitute-new-for-old-book new-book orig))
			)
		   (begin
		     (backup-json "books")
		     (send-json-to "books" mod-all-books))))		 		 
 		 ((string= action "w") 
		  (let* (
	      		 (orig (car (get-book-with-id id)))
		;	 (dummy (pretty-print orig))
			)
		  (move-to-withdraw orig))			  
		 )
		)))))


(define (query-by-title)
  ;;
  ;;this is a menu item
  (let* ((dummy (display-logo))
	 (in (readline "Select title: "))
	 (lst (get-books-with-title in))
	 )
    (if (= (length lst) 0)
	(display "Match not found!\n\n")
	(let* ((dummy (display-results-header))
	      ; (dummy (pretty-print lst))
	       (dummy (display-results (reverse lst)))
	       
	       (dummy (display "\nActivities: t - add tag"))
	       (dummy (display "\n            w - withdraw"))
	       (dummy (display "\nIndicate as a doublet e.g  '1 t' to add tags to book 1"))	       
	       (what-do  (readline "\nAction: [1 t]: "))
	       (a (string-split what-do #\space))
               (abook  (find-element-with-counter lst (car a)))
	       (dummy (pretty-print abook))
	       (id (assoc-ref abook "id"))
	       (action (cadr a))	       
	       )	  
	  (cond ((string= action "t")
		 (let* ((dummy (display-tag-menu))
			(the-tags (query-for-tags))
			(new-book (assign-tags-to-book id the-tags))
			(orig (car (get-book-with-id id)))
			(mod-all-books (substitute-new-for-old-book new-book orig))
			)
		   (begin
		     (backup-json "books")
		     (send-json-to "books" mod-all-books))))		 
 		 ((string= action "w") 
		  (let* (
	      		 (orig (car (get-book-with-id id)))
		;	 (dummy (pretty-print orig))
			)
		  (move-to-withdraw orig))			  
		 )
		)))))



(define (display-results-header)
  (begin
    (display "\n   |           Title               |           Author              |\n")
    (display "---|-------------------------------|-------------------------------|\n")))

(define (display-results lst )
  ;; 1 a|title b |author c |
  ;;
  (if (null? (cdr lst))
      (let* ((title (assoc-ref (car lst) "title"))
	     (title (if (>= (string-length  title) 30) (substring title 0 29) title))
	     (title-len (string-length  title))	     
	     (author (car (vector->list (assoc-ref (car lst) "author"))))
	     (author (if (>= (string-length  author) 30) (substring author 0 29) author))
	     (author-len (string-length  author))
	     (counter  (assoc-ref (car lst) "counter"))
	     (counter-len (string-length  counter))
	     (a (make-string (- 3 (string-length  counter)) #\space))
	     (b (make-string (- 30 (string-length  title)) #\space))
	     (c (make-string (- 30 (string-length  author)) #\space))     
	     (dummy (display (string-append  counter a "|" title b " | " author c "|\n"))))
	#t)
      (let* ((title (assoc-ref (car lst) "title"))
	     (title (if (>= (string-length  title) 30) (substring title 0 29) title))
	     (title-len (string-length  title))
	     (author (car (vector->list (assoc-ref (car lst) "author"))))
	     (author (if (>= (string-length  author) 30) (substring author 0 29) author))
	     (author-len (string-length  author))
	     (counter (assoc-ref (car lst) "counter"))
	     (counter-len (string-length  counter))
	     (a (make-string (- 3 (string-length  counter)) #\space))
	     (b (make-string (- 30 (string-length  title)) #\space))
	     (c (make-string (- 30 (string-length  author)) #\space))     
	     (dummy (display (string-append  counter a "|" title b " | " author c "|\n")))
	    ; (dummy (set! counter (+ counter 1)))
	     )
	(display-results (cdr lst))) ))



