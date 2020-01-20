#lang racket

(require "bibgen.rkt"
         "bib-write.rkt")

;; tests
(module+ test

  (parameterize ([BIBDIR "~/git/biber-dist/"])

    ;; AI
    (gen-bib-and-write 'icml 2019)
    (gen-bib-and-write 'naacl 2019)
    
    (gen-bib-and-write 'aistats 2019)
    (gen-bib-and-write 'cvpr 2019)
    (gen-bib-and-write 'iclr 2019)
    (gen-bib-and-write 'iclr 2020)

    ;; system
    (for ([year (in-range 2015 2020)])
      (gen-bib-and-write 'isca year))
    (for ([year (in-range 2000 2019)])
      (gen-bib-and-write 'iccad year))
    (for ([year (in-range 2000 2020)])
      (gen-bib-and-write 'dac year))
    (for ([year (in-range 2000 2020)])
      (gen-bib-and-write 'ispd year))

    ;; PL
    (gen-bib-and-write 'popl 2018)
    (gen-bib-and-write 'popl 2019)
    (gen-bib-and-write 'pldi 2019)
    (gen-bib-and-write 'icfp 2019)
    (gen-bib-and-write 'icfp 2018)


    ;; arxiv
    (for ([cat (list "cs.AI" "cs.CV" "cs.LG" "stat.ML"
                     ;; "cs.PL" "cs.RO"
                     )]
          #:when #t
          [year (in-range 2017 2019)]
          #:when #t
          [month (in-range 1 13)])
      (gen-bib-and-write-arxiv cat year month arxiv-bib))
    (for ([cat (list "cs.AI" "cs.CV" "cs.LG" "stat.ML")]
          #:when #t
          [month (in-range 1 12)])
      (gen-bib-and-write-arxiv cat 2019 month arxiv-bib))
    
    (for ([m (in-range 1 13)])
      (gen-bib-and-write-arxiv "cs.AI" 2018 m arxiv-bib))
    (for ([m (in-range 1 7)])
      (gen-bib-and-write-arxiv "cs.AI" 2019 m arxiv-bib))))
