#lang racket

(require net/url)
(require racket/port)
(require racket/format)
;; needs installing
(require html-parsing)
;; needs installing
(require sxml)
(require roman-numeral)
;; (require file/sha1)
(require sha)
(require racket/file)

(provide maybe-retrieve-url paper->bib pref->bibpath)


(define (download url f)
  "Should not use directly."
  (displayln (~a "downloading " f))
  (make-parent-directory* f)
  (let* ([in (get-pure-port
              (string->url url))]
         [out (open-output-file f #:exists 'replace)])
    (write-bytes
     (port->bytes in)
     out)
    (close-input-port in)
    (close-output-port out)
    (displayln (~a "Downloaded " url " to " f))))

(module+ test
  (download "http://lihebi.com/cv.pdf" "tmp/cv.pdf")
  (download "http://lihebi.com/index.html" "tmp/index.html"))


(define download-with-delay
  (let ([ct 0])
    (λ (url filename)
      "Sleep 5 sec every 10 downloads."
      (set! ct (add1 ct))
      (when (= (modulo ct 10) 0)
        (displayln "Sleeping for 5 sec ..")
        (sleep 5))
      (download url filename))))

(module+ test
  (for ([id (range 25)])
    (download-with-delay "http://lihebi.com/cv.pdf" "tmp/cv.pdf")))

(define (url->local-filename url)
  (string-append
   "tmp/"
   (bytes->hex-string
    (sha256 (string->bytes/locale url)))))

(module+ test
  (url->local-filename "https://lihebi.com"))

(define (maybe-retrieve-url url)
  "Download url to a local file, and return the filename."
  (let ([filename (url->local-filename url)])
    (when (not (file-exists? filename))
      (download-with-delay url filename))
    filename))

(module+ test
  (maybe-retrieve-url "http://lihebi.com/cv.pdf"))

(define (title-first-word title)
  (let ([lst (map string-downcase
                  (string-split title #px"\\s+|:|-|\\?|\\(|\\)|/|,|'|\\*|\""))])
    (let ([kw-filter (λ (x)
                       (not
                        (member
                         x
                         '("a" "i" "you" "he"
                               "the" "and"
                               "an" "on" "can"
                               "where" "when" "how" "what" "who" "why"
                               "do" "does" "to" "for" "is" "are" "don" "t"
                               "from"))))]
          [length-filter (λ (s) (> (string-length s) 2))])
      (let ([filtered-lst (filter
                           length-filter
                           (filter
                            kw-filter
                            (filter
                             non-empty-string?
                             lst)))])
        (if (empty? filtered-lst) "Title"
            (string-titlecase (first filtered-lst)))))))
(module+ test
  (title-first-word "a-(\"World*,/)?: haha")
  )

(define (gen-id title authors pdflink pref)
  (string-replace
   (string-replace
    (~a
     pref "-"
     (let ([first-author (first authors)])
       (last (string-split first-author)))
     "-"
     (title-first-word title))
    "." "")
   "'" ""))

(define (pref->year p)
  (second
   (regexp-match #px"([0-9]{4})" p)))
(define (pref->conf p)
  (second (regexp-match #px"[0-9]{4}-([a-zA-Z]*)" p)))

(define (paper->bib title authors pdflink pref)
  (define year (pref->year pref))
  (define conf (pref->conf pref))
  (~a "@inproceedings{" (gen-id title authors pdflink pref) ",\n"
      (string-join
       (list (~a "author={" (string-join authors " and ") "}")
             (~a "title={" title "}")
             (~a "year={" year "}")
             (~a "booktitle={" conf "}")
             (~a "pdflink={" pdflink "}"))
       ",\n")
      "\n}"))

(define (pref->bibpath pref)
  (string-append (pref->conf pref)
                 "/" pref ".bib"))


