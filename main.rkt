#lang racket

(require file/sha1
         net/url
         sxml
         html-parsing
         rackunit
         "newutils.rkt")


(define (pmlr-bib volume booktitle year)
  ;; PMLR http://proceedings.mlr.press/
  (define pmlr-prefix "http://proceedings.mlr.press/")
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

(define (gen-aistats year)
  (case year
    [(2019) (pmlr-bib 89 "AISTATS" 2019)]))

(define (thecvf-bib suffix booktitle year)
  ;; http://openaccess.thecvf.com/CVPR2019.py
  (define thecvf-prefix "http://openaccess.thecvf.com/")
  (define suffix "CVPR2019.py")
  (define xexp (port->xexp (open-url-port
                            (string-append thecvf-prefix suffix))))

  (define titles ((sxpath "//dt/a/text()") xexp))
  (define dts ((sxpath "//dd") xexp))

  (when (not (= (* (length titles) 2)
                (length dts)))
    (error (format "Elements not match: ~a titles, ~a dts"
                   (length titles) (length dts))))

  (define (get-papers titles dts)
    (if (empty? titles) '()
        (let* ([title (first titles)]
               [authors (map string-trim ((sxpath "//form/a/text()") (first dts)))]
               [pdflink (string-append
                         thecvf-prefix
                         (first ((sxpath "/a[text()='pdf']/@href/text()") (second dts))))]
               [p (paper title authors pdflink booktitle year)])
          (cons p (get-papers (rest titles) (rest (rest dts)))))))
  (string-join (map gen-single-bib (get-papers titles dts))))

(define (gen-cvpr year)
  (case year
    [(2019) (thecvf-bib "CVPR2019.py" "CVPR" 2019)]))
 

;; tests
(module+ test

  #;
  (with-output-to-file "tmp.txt"
    (λ () (displayln xexp)))

  (parameterize ([BIBDIR "/home/hebi/github/biber-dist/"])
    (gen-bib-and-write "ICML" 2019 gen-icml)
    (gen-bib-and-write "AISTATS" 2019 gen-aistats)
    (gen-bib-and-write "CVPR" 2019 gen-cvpr))


  )
