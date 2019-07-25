#lang racket

(require file/sha1
         net/url
         sxml
         html-parsing
         rackunit)

(provide port->xexp
         open-url-port
         gen-single-bib
         paper
         gen-bib-and-write
         gen-bib-and-write-arxiv
         BIBDIR)


(define (port->xexp in)
  (begin0
      (html->xexp in)
    (close-input-port in)))

(define (string-hash str)
  (bytes->hex-string (sha1-bytes (open-input-string str))))

(define controlled-port->string
  ;; There is a speed control parameter. By default it is 5/15
  ;; seconds.
  (let ([num 5]
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
          (set! l (take-right (append l (list current-seconds)) num)))))))

(define (open-url-port url)
  ;; hash the url
  ;; download to tmp/xxx if does not exist
  ;; (bytes->hex-string (string->bytes/locale "feis"))
  ;;
  ;; return an input port for reading
  (let ([out-fname (string-append "tmp/" (string-hash url))])
    (when (not (file-exists? out-fname))
      (let ([in (get-pure-port
                 (string->url url)
                 #:redirections 5)]
            [out (open-output-file out-fname)])
        (write-string
         (controlled-port->string in)
         out)
        (close-input-port in)
        (close-output-port out)))
    (open-input-file out-fname)))


(define (clean-string s)
  (string-trim (string-replace s #px"\\s+" " ")))

(check-equal? (clean-string " Hello  World  ") "Hello World")

(struct paper
  (title authors pdflink booktitle year))

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
                       (displayln (format "Warning: title empty " title))
                       "Title")
      (string-titlecase (first words))))


(define (clean-id id)
  (string-replace id "’" ""))

(module+ test
  (clean-id "2019-AISTATS-D’Amour-Multi"))

(define (gen-single-bib p)
  (define id (clean-id
              (string-join (list (number->string (paper-year p))
                                 (paper-booktitle p)
                                 (last (string-split (first (paper-authors p))))
                                 (title-first-word (paper-title p)))
                           "-")))
  (define author (string-join (paper-authors p) ", "))
  (define year (paper-year p))
  (string-append "@inproceedings{" id ",\n"
                 "  title={" (clean-string (paper-title p)) "},\n"
                 "  author={" (clean-string author) "},\n"
                 "  year={" (number->string (paper-year p)) "},\n"
                 "  booktitle={" (paper-booktitle p) "},\n"
                 "  pdflink={" (paper-pdflink p) "}\n}\n"))

(define BIBDIR (make-parameter "bib"))

(define (gen-bib-and-write conf year bibfunc #:overwrite [overwrite #f])
  (define bibdir (string-append (BIBDIR) "/" conf))
  (when (not (file-exists? bibdir))
    (make-directory* bibdir))
  (let ([f (string-append bibdir "/" conf "-" (number->string year) ".bib")])
    (when (or (not (file-exists? f))
              overwrite)
      (displayln "Generating bib ..")
      (let ([output (bibfunc year)])
        (displayln (format "Writing to ~a ..." f))
        (with-output-to-file f
          (λ () (displayln output))
          #:exists 'replace)))))

(define (gen-bib-and-write-arxiv cat year month bibfunc #:overwrite [overwrite #f])
  (define bibdir (string-append (BIBDIR) "/" cat))
  (when (not (file-exists? bibdir))
    (make-directory* bibdir))
  (let ([f (string-append bibdir "/" cat "-" (number->string year) "-"
                          (~a month
                              #:width 2 #:pad-string "0" #:align 'right)
                          ".bib")])
    (when (or (not (file-exists? f))
              overwrite)
      (displayln "Generating bib ..")
      (let ([output (bibfunc cat year month)])
        (displayln (format "Writing to ~a ..." f))
        (with-output-to-file f
          (λ () (displayln output))
          #:exists 'replace)))))
