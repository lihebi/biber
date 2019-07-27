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
     (gen-single-bib (paper title authors pdflink booktitle year)))
   "\n"))

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
  (string-join (map gen-single-bib (get-papers titles dts)) "\n"))

(define (gen-cvpr year)
  (case year
    [(2019) (thecvf-bib "CVPR2019.py" "CVPR" 2019)]))

;; https://arxiv.org/list/cs.AI/1905
;; https://arxiv.org/list/cs.AI/1905?show=99999

;; arXiv

;; I would probably organize the papers by month
;; I'm going to download only papers after 2018
;; The naming: 2018-arXiv-XXX-YYY
;; The file naming: 2018-01-arXiv.bib

(define (arxiv-bib cat year month)
  (define (arxiv-query-date year month)
    (string-append
     (~a (modulo year 100)
         #:width 2 #:pad-string "0" #:align 'right)
     (~a month
         #:width 2 #:pad-string "0" #:align 'right)))
  
  (check-equal? (arxiv-query-date 2019 7) "1907")
  (check-equal? (arxiv-query-date 1995 11) "9511")
  (check-equal? (arxiv-query-date 2000 2) "0002")

  (define url (format "https://arxiv.org/list/~a/~a?show=999999"
                      cat (arxiv-query-date year month)))
  (println url)
  (define xexp (port->xexp (open-url-port url)))
  (define IDs (map (λ (s)
                     (last (string-split s "/")))
                   ((sxpath "//dt/span/a[2]/@href/text()") xexp)))
  (define titles (map (λ (x)
                        (string-join
                         (filter non-empty-string?
                                 (map string-trim
                                      ((sxpath "//div/text()") (list '*TOP* x))))
                         "SHOULD_NOT_BE_HERE"))
                      ((sxpath "//dd/div/div[contains(@class, 'list-title')]") xexp)))
  (define authors (map (λ (x)
                         ((sxpath "//a/text()") x))
                       ((sxpath "//dd/div/div[contains(@class, 'list-authors')]") xexp)))
  (when (not (= (length IDs) (length titles) (length authors)))
    (error "ID title author numbers do not match"))
  (displayln (format "Total paper: ~a" (length IDs)))
  (define papers (for/list ([ID IDs]
                            [title titles]
                            [author authors])
                   (let ([pdflink (format "https://arxiv.org/pdf/~a.pdf" ID)])
                     (paper title author pdflink cat year))))
  (string-join (map gen-single-bib papers) "\n"))

(define (xexp-get-all-text x)
    (match x
      [(list '@ child ...) ""]
      [(list name child ...) (string-join (map xexp-get-all-text child) "")]
      [s s]))

(define (acl-bib conf year)
  (define acl-prefix "https://aclanthology.info/events/")
  (define suffix (string-append (string-downcase conf) "-" (number->string year)))
  (define url (string-append acl-prefix suffix))
  (define xexp (port->xexp (open-url-port url)))

  (define paper-xexps ((sxpath "//div[@id='n19-1']/p") xexp))

  (let ([titles (map (λ (x)
                       (string-join (map xexp-get-all-text ((sxpath "//span/strong/a") x)) ""))
                     paper-xexps)]
        [authors (map (λ (x) ((sxpath "//span[2]/a/text()") x))
                      paper-xexps)]
        [pdflinks (map (λ (x) (first ((sxpath "//span[1]/a[1]/@href/text()") x)))
                       paper-xexps)])
    (define papers (for/list ([pdflink pdflinks]
                              [title titles]
                              [author authors])
                     (paper title author pdflink conf year)))
    (string-join (map gen-single-bib papers) "\n")))

(define (gen-naacl year)
  (case year
    [(2019) (acl-bib "NAACL" 2019)]))


;; tests
(module+ test

  (void (arxiv-bib "cs.AI" 2017 1))

  #;
  (with-output-to-file "tmp.txt"
    (λ () (displayln xexp)))

  (parameterize ([BIBDIR "/home/hebi/github/biber-dist/"])
    (gen-bib-and-write "ICML" 2019 gen-icml)
    
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
