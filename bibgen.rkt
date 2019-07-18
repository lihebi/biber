#lang racket

(require file/sha1
         net/url
         sxml
         html-parsing
         rackunit
         "newutils.rkt")

;; generate bib for conferences

;; PMLR http://proceedings.mlr.press/

(define pmlr-prefix "http://proceedings.mlr.press/")
(define (pmlr-bib volume booktitle year)
  (define xexp (port->xexp (open-url-port
                            (string-append pmlr-prefix "v" (number->string volume)))))
  (string-join
   (for/list ([p ((sxpath "//div[@class='paper']") xexp)])
     (define title (first ((sxpath "//p[@class='title']/text()") p)))
     (define authors (filter non-empty-string?
                             (map (λ (s) (string-trim (string-replace s #rx"\n|," "")))
                                  ((sxpath "//span[@class='authors']/text()") p))))
     (define pdflink (second ((sxpath "//a/@href/text()") p)))
     (gen-single-bib (paper title authors pdflink booktitle year)))))

(define (gen-icml year)
  (case year
    [(2019) (pmlr-bib 97 "ICML" 2019)]))
 

;; tests
(define (test)

  (parameterize ([BIBDIR "/home/hebi/github/biber-dist/"])
   (gen-bib-and-write "ICML" 2019 gen-icml))

  )
