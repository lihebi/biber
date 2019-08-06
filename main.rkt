#lang racket

(require file/sha1
         net/url
         sxml
         html-parsing
         rackunit
         "utils.rkt"
         "bibgen.rkt")


(define BIBDIR (make-parameter "bib"))

(define (gen-bib-and-write conf year #:overwrite [overwrite #f])
  (define conf-str (string-upcase (symbol->string conf)))
  (define bibdir (string-append (BIBDIR) "/" conf-str))
  (when (not (file-exists? bibdir))
    (make-directory* bibdir))
  (let ([f (string-append bibdir "/" conf-str "-" (number->string year) ".bib")])
    (when (or (not (file-exists? f))
              overwrite)
      (displayln (format "Generating bib for {} {} .." conf year))
      (let ([output (gen-bib conf year)])
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

;; tests
(module+ test

  (parameterize ([BIBDIR "/home/hebi/github/biber-dist/"])

    ;; AI
    (gen-bib-and-write 'icml 2019)
    (gen-bib-and-write 'naacl 2019)
    
    (gen-bib-and-write 'aistats 2019)
    (gen-bib-and-write 'cvpr 2019)

    ;; system
    (for ([year (in-range 2015 2020)])
      (gen-bib-and-write 'isca year))
    (for ([year (in-range 2015 2019)])
      (gen-bib-and-write 'iccad year))
    (for ([year (in-range 2012 2020)])
      (gen-bib-and-write 'dac year))

    ;; PL
    (gen-bib-and-write 'popl 2018)
    (gen-bib-and-write 'popl 2019)
    (gen-bib-and-write 'pldi 2019)
    (gen-bib-and-write 'icfp 2019)


    ;; arxiv
    (for ([cat (list "cs.AI" "cs.CV" "cs.LG"
                     ;; "cs.PL" "cs.RO"
                     )]
          #:when #t
          [year (in-range 2017 2019)]
          #:when #t
          [month (in-range 1 13)])
      (gen-bib-and-write-arxiv cat year month arxiv-bib))
    (for ([cat (list "cs.AI" "cs.CV" "cs.LG")]
          #:when #t
          [month (in-range 1 8)])
      (gen-bib-and-write-arxiv cat 2019 month arxiv-bib))
    
    (for ([m (in-range 1 13)])
      (gen-bib-and-write-arxiv "cs.AI" 2018 m arxiv-bib))
    (for ([m (in-range 1 7)])
      (gen-bib-and-write-arxiv "cs.AI" 2019 m arxiv-bib))))
