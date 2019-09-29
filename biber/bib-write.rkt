#lang racket

(require file/sha1
         net/url
         sxml
         html-parsing
         rackunit
         "utils.rkt"
         "bibgen.rkt")

(provide gen-bib-and-write
         gen-bib-and-write-arxiv
         BIBDIR)

(define BIBDIR (make-parameter "bib"))

(define (gen-bib-and-write conf year #:overwrite [overwrite #f])
  (let* ([conf-str (string-upcase (symbol->string conf))]
         [bibdir (build-path (expand-user-path (BIBDIR)) conf-str)]
         [f (build-path bibdir (string-append conf-str "-" (number->string year) ".bib"))])
    (when (or (not (file-exists? f))
              overwrite)
      (make-parent-directory* f)
      (displayln (format "Generating bib for ~a ~a .." conf year))
      (let ([output (gen-bib conf year)])
        (displayln (format "Writing to ~a ..." f))
        (with-output-to-file f
          (λ () (displayln output))
          #:exists 'replace)))))

(define (gen-bib-and-write-arxiv cat year month bibfunc #:overwrite [overwrite #f])
  (let* ([bibdir (build-path (expand-user-path (BIBDIR)) cat)]
         [f (build-path bibdir (string-append cat "-" (number->string year) "-"
                                              (~a month
                                                  #:width 2 #:pad-string "0" #:align 'right)
                                              ".bib"))])
    (when (or (not (file-exists? f))
              overwrite)
      (make-parent-directory* f)
      (displayln "Generating bib ..")
      (let ([output (bibfunc cat year month)])
        (displayln (format "Writing to ~a ..." f))
        (with-output-to-file f
          (λ () (displayln output))
          #:exists 'replace)))))

