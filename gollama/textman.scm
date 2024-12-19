(define-module (gollama textman) 
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
 #:use-module (rnrs io ports) ;;string->bytevector
 #:use-module (ice-9 textual-ports)
 #:use-module (ice-9 ftw);;scandir
 #:use-module (ice-9 format)
 #:use-module (gcrypt hash)
 #:use-module (ice-9 binary-ports)
 #:use-module (ice-9 match)
 #:use-module (gcrypt hash)
 #:use-module (gcrypt base16)
 #:use-module (gollama db)
 #:use-module (gollama ollama)
 #:use-module (gollama utilities)
 #:export (
	   process-prompt-files
	   collect-paragraphs
	   recurse-paragraphs-for-ids
	   provide-indices
	   count-tokens
	   ))

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

(define (provide-indices lst counter)
  ;;query prompts provide index
   (fold (lambda (x prev)
	   (begin
	     (set! counter (+ counter 1))		 	 	  
	     (acons (number->string counter) x  prev)
	     ))
        '() lst))



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


(define (collect-paragraphs file)
  ;;file is full path
  (define paragraph "")
  (define para-lst '())
  (let* (
	 (port (open-input-file file))
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


(define (process-prompt-files id top-dir)
  ;;file is file name only no path
  ;;guix shell -m manifest.scm -- guile -L . -l "gollama.scm" -c '(process-prompt-files "e210c4f856f0" "/home/mbc/projects/gollama")'
  ;;guile -L . -l "gollama.scm" -c '(ingest "ppan.txt" "/home/ubuntu/gollama")'
   
  (let* (;;(orig-text-file (string-append top-dir "/text/" file))
	 (query-file (string-append top-dir "/text/queries.json"))
	 (query-flag (access? query-file F_OK))
	 (_ (if query-flag (let* ((a (get-list-from-json-file query-file #f))
				  (b (provide-indices a 0)))
			     (begin
			       (save-to-json b (string-append top-dir "/db/" id "/indexed-queries.json" ) #t)
			       (copy-file query-file (string-append top-dir "/db/" id "/queries.json" ))))))
	 
	 (sys-prompt-file (string-append top-dir "/text/system-prompts.json"))
	 (sys-p-flag (access? sys-prompt-file F_OK))
	 (_ (if sys-p-flag (let* ((a (get-list-from-json-file sys-prompt-file #f))
				  (b (provide-indices a 0)))
			     (begin
			       (save-to-json b (string-append top-dir "/db/" id "/indexed-system-prompts.json" ) #t)
			       (copy-file sys-prompt-file (string-append top-dir "/db/" id "/system-prompts.json" ))))))

	 (contexts-file (string-append top-dir "/text/contextss.json"))
	 (sys-p-flag (access? contexts-file F_OK))
	 (_ (if sys-p-flag (let* ((a (get-list-from-json-file contexts-file #f))
				  (b (provide-indices a 0)))
			     (begin
			       (save-to-json b (string-append top-dir "/db/" id "/indexed-contextss.json" ) #t)
			       (copy-file contexts-file (string-append top-dir "/db/" id "/contexts.json" ))))))


	 )
    #f))
