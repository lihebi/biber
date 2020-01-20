#lang racket

(require file/sha1
         net/url
         net/cookies
         net/head
         json
         syntax/parse/define
         sxml
         html-parsing
         rackunit)

(provide
 url->xexp
 my/url->string
 file->xexp
 url->json
 gen-single-bib
 paper
 comment)

(define-simple-macro (comment stx ...)
  (void))

(define (port->xexp in)
  (begin0
      (html->xexp in)
    (close-input-port in)))

(define (string-hash str)
  (string-append (bytes->hex-string (sha1-bytes (open-input-string str)))
                 "-" (substring (string-replace str "/" "-") 0 20)))

(define controlled-port->string
  ;; There is a speed control parameter. By default it is 3/15
  ;; seconds.
  (let ([num 3]
        [sec-limit 15])
    (let ([l (make-list num 0)])
      (lambda (in)
        (let ([cur-secs (- (current-seconds) (first l))])
          (when (< cur-secs sec-limit)
            (displayln (format "Reading url too fast, sleeping for ~a seconds .."
                               (- sec-limit cur-secs)))
            (sleep (- sec-limit cur-secs))))
        (begin0
            (port->string in)
          (set! l (take-right (append l (list (current-seconds))) num)))))))

(define (get-cookie url)
  ;; (cookie-header (string->url "https://lihebi.com"))
  (when (not (cookie-header (string->url url)))
    ;; 1. get a cookie
    (extract-and-save-cookies!
     (map string->bytes/locale
          (string-split
           (purify-port
            (get-impure-port
             (string->url url))) "\n"))
     (string->url url)))
  ;; get the cookie header
  (let ([c (cookie-header (string->url url))])
    (if c
        (string-append
         "Cookie: "
         (bytes->string/locale c))
        "")))

(define (open-url-port url)
  ;; hash the url
  ;; download to tmp/xxx if does not exist
  ;; (bytes->hex-string (string->bytes/locale "feis"))
  ;;
  ;; return an input port for reading
  (let ([out-fname (string-append "/tmp/biber/" (string-hash url))])
    (when (not (file-exists? out-fname))
      (make-parent-directory* out-fname)
      (let ([in (get-pure-port
                 (string->url url)
                 ;; get cookie if not available
                 (list (get-cookie url))
                 #:redirections 5)]
            [out (open-output-file out-fname)])
        (write-string
         (controlled-port->string in)
         out)
        (close-input-port in)
        (close-output-port out)))
    (open-input-file out-fname)))

(define (my/url->string url)
  (let ([port (open-url-port url)])
    (begin0 (port->string port)
      (close-input-port port))))

(define (url->xexp url)
  "Download url to /tmp/xxx and parse it to xexp.

This function uses open-url-port, and close port automatically."
  (let ([port (open-url-port url)])
    (begin0 (port->xexp port)
      (close-input-port port))))

(define (url->json url)
  (let ([port (open-url-port url)])
    (begin0 (read-json port)
      (close-input-port port))))

(define (file->xexp path)
  (call-with-input-file
    (λ (port)
      (port->xexp port))))


(define (clean-string s)
  (string-trim (string-replace s #px"\\s+" " ")))

(check-equal? (clean-string " Hello  World  ") "Hello World")

(struct paper
  ;; TODO add contract
  (title authors pdflink booktitle year)
  #:prefab)

(define (title-first-word title)
  (define stop-words '("you" "the" "and" "can"
                       "where" "when" "how" "what" "who" "why"
                       "does" "for" "are" "don" "from"))
  (define (word-filter x)
    (and (non-empty-string? x)
         (not
          (member x stop-words))
         (> (string-length x) 2)))
  (define words (filter word-filter
                        (map string-downcase
                             (string-split title #px"\\s+|:|-|\\?|\\(|\\)|/|,|'|\\*|\""))))
  (if (empty? words) (begin
                       (displayln (~a "Warning: title empty " title))
                       "Title")
      (string-titlecase (first words))))


(define (clean-id id)
  ;; There maybe * in author name
  (string-replace id #rx"’|\\+|\\.|\\*" ""))

(module+ test
  (clean-id "’hello+"))

(module+ test
  (clean-id "2019-AISTATS-D’Amour-Multi"))

(define (gen-single-bib p)
  (define id (clean-id
              (string-join (list (number->string (paper-year p))
                                 (paper-booktitle p)
                                 (last (string-split (first (paper-authors p))))
                                 (title-first-word (paper-title p)))
                           "-")))
  (define author (string-join (paper-authors p) " and "))
  (define year (paper-year p))
  (~a "@inproceedings{" id ",\n"
      ;; TODO sometimes the title is all upper case (such as some in
      ;; ICLR). I probably want to clean this up.
      "  title={" (clean-string (paper-title p)) "},\n"
      "  author={" (clean-string author) "},\n"
      "  year={" (number->string (paper-year p)) "},\n"
      "  booktitle={" (paper-booktitle p) "},\n"
      ;; FIXME if pdflink is #f, this should just output #f
      "  pdflink={" (paper-pdflink p) "}\n}\n"))

