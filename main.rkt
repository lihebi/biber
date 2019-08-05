#lang racket

(require file/sha1
         net/url
         sxml
         html-parsing
         rackunit
         "utils.rkt"
         "bibgen.rkt")

;; tests
(module+ test

  (parameterize ([BIBDIR "/home/hebi/github/biber-dist/"])
    (gen-bib-and-write "ICML" 2019 gen-icml)
    (gen-bib-and-write "ICFP" 2019 gen-icfp)

    (for ([year (in-range 2015 2020)])
      (gen-bib-and-write "ISCA" year gen-isca))
    (for ([year (in-range 2015 2019)])
      (gen-bib-and-write "ICCAD" year gen-iccad))
    (for ([year (in-range 2015 2020)])
      (gen-bib-and-write "DAC" year gen-dac))
    
    
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
      (gen-bib-and-write-arxiv "cs.AI" 2019 m arxiv-bib))

    (gen-bib-and-write "NAACL" 2019 gen-naacl)
    
    (gen-bib-and-write "AISTATS" 2019 gen-aistats)
    (gen-bib-and-write "CVPR" 2019 gen-cvpr))


  )
