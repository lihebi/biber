#lang racket

(require net/url)
(require racket/port)
(require racket/format)
;; needs installing
(require html-parsing)
;; needs installing
(require sxml)
(require roman-numeral)

(require html-parsing)

(define (download-html-direct url f)
  (let* ([in (get-pure-port
              (string->url url))]
         [out (open-output-file f #:exists 'replace)])
    (displayln (~a "downloading " f))
    (write-string
     (port->string in)
     out)
    (close-input-port in)
    (close-output-port out)
    (displayln (~a "Downloaded " url " to " f))))

(define (download-html url f
                       #:exists [exists-flag 'skip]
                       #:timeout [timeout 0])
  (if (file-exists? f)
      (case exists-flag
        [(error) (error "error: file exists")]
        [(replace) (begin
                     (download-html-direct url f)
                     (sleep timeout))]
        [(skip) (displayln "skip")]
        [else (error "exsits-flag error")])
      (begin
        (download-html-direct url f)
        (sleep timeout))))



;; https://aaai.org/Library/AAAI/aaai17contents.php

(define (download-aaai year)
  (let* ([2-digit-year (~a (modulo year 100)
                           #:width 2 #:pad-string
                           "0" #:align 'right)]
         [url (~a "https://aaai.org/Library/AAAI/"
                  "aaai" 2-digit-year
                  "contents.php")]
         [filename (~a "html/AAAI/AAAI-" year ".html")])
    (download-html url filename)))

;; 2010 - 2018
(define (download-aaai-recent year)
  (let* ([2-digit-year (~a (modulo year 100)
                           #:width 2 #:pad-string
                           "0" #:align 'right)]
         ;; https://www.aaai.org/ocs/index.php/AAAI/AAAI12/schedConf/presentations
         [url (~a "https://www.aaai.org/ocs/index.php/AAAI/AAAI"
                  2-digit-year
                  "/schedConf/presentations")]
         [filename (~a "html/AAAI/AAAI-" year ".html")])
    (download-html url filename #:exists 'replace)))

(define (download-aaai-all)
  #;
  (for ([year (range 1980 2010)])
    (download-aaai year)
    (sleep 5))
  (for ([year (range 2010 2019)])
    (download-aaai-recent year)
    (displayln "sleeping 5 sec ..")
    (sleep 5)))

(define (dwonload-jmlr-all)
  "http://www.jmlr.org/papers/v1"
  (for ([v (range 1 19)]
        [year (range 2000 2018)])
    (download-html
     (~a "http://www.jmlr.org/papers/v" v "/index.html")
     (~a "html/JMLR/JMLR-" year ".html")
     #:exists 'replace
     #:timeout 5)))

(module+ test
  (download-html "https://aaai.org/Library/conferences-library.php"
                 "test.php")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; download AAAI htmls
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (download-aaai 1999)
  (for ([year (range 2000 2018)])
    (download-aaai year)
    (sleep 5))

  ;; extract bib
  ;; downloading pdf

  ;; change Library to Papers
  ;; change extension to .pdf
  
  ;; https://aaai.org/Library/AAAI/2004/aaai04-001.php
  ;; https://aaai.org/Papers/AAAI/2004/AAAI04-001.pdf
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; UAI
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (download-html "https://dslpitt.org/uai/displayArticles.jsp?mmnu=1&smnu=1&proceeding_id=1" "test.html")

  ;; 1985-2014
  (for ([id (range 1 31)]
        [year (range 1985 2015)])
    (let ([filename (~a "html/UAI/" "UAI-" year ".html")])
      (download-html
       (~a "https://dslpitt.org/uai/displayArticles.jsp?mmnu=1&smnu=1&proceeding_id=" id)
       filename)
      (sleep 5)))

  ;; 2015-2017 are hosted on their own websites
  (for ([url (list
              ;; "http://auai.org/uai2012/acceptedPapers.shtml"
              ;; "http://auai.org/uai2013/acceptedPapers.shtml"
              ;; "http://auai.org/uai2014/acceptedPapers.shtml"
              "http://auai.org/uai2015/proceedings.shtml"
              "http://auai.org/uai2016/proceedings.php"
              "http://auai.org/uai2017/accepted.php")]
        [year (range 2015 2018)])
    (let ([filename (~a "html/UAI/" "UAI-" year ".html")])
      (download-html url filename)
      (sleep 5)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; NIPS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  "https://nips.cc/Conferences/2008"

  "https://papers.nips.cc/book/advances-in-neural-information-processing-systems-30-2017"
  "https://papers.nips.cc/book/advances-in-neural-information-processing-systems-1-1988"

  (define xexp (html->xexp (open-input-file "./html/AAAI/AAAI-2012.html")))
  ((sxpath '(// div (class "content")))
   xexp)

  ((sxpath '(@ class)) '(b (@ (class "c")) "xxx"))

  (filter
   (λ (x)
     (not (empty? ((sxpath '(@ class)) x))))
   ((sxpath '(// b))
           (html->xexp (open-input-string "<a><b class=\"c\">xxx</b><b>cc</b></a>"))))

  (for ([id (range 1 31)]
        [year (range 1988 2018)])
    (let ([filename (~a "html/NIPS/NIPS-" year ".html")]
          [url (~a "https://papers.nips.cc/book/advances-in-neural-information-processing-systems-" id "-" year)])
      (download-html url filename)
      (sleep 5)))

  (download-html "https://papers.nips.cc/book/neural-information-processing-systems-1987"
                 "html/NIPS/NIPS-1987.html")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ICML
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; 04-09: acm
  (for ([proc '(("https://icml.cc/Conferences/2010/abstracts.html" 2010)
                ("https://icml.cc/Conferences/2011/papers.php.html" 2011)
                ("https://icml.cc/Conferences/2012/papers.1.html" 2012))])
    (let* ([url (first proc)]
           [year (second proc)]
           [filename (~a "html/ICML/ICML-" year ".html")])
      (download-html url filename)
      (sleep 5)))

  (for ([vl (list 28 32 37 48 70 80)]
        [year (range 2013 2019)])
    (let ([url (~a "http://proceedings.mlr.press/v" vl "/index.html")]
          [filename (~a "html/ICML/ICML-" year ".html")])
      (download-html url filename #:exists 'skip)
      #;(sleep 5)
      ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ACML
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; ACML 09 is the first, but it is not in mlr website, but on
  ;; Springer, omit those papers for now
  (for ([vl (list 13 20 25 29 39 45 63 77)]
        [year (range 2010 2018)])
    (let ([url (~a "http://proceedings.mlr.press/v"
                   vl "/index.html")]
          [filename (~a "html/ACML/ACML-" year ".html")])
      (download-html url filename #:exists 'replace)
      (sleep 5)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; COLT: Conference on Learning Theory
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (for ([vl (list 19 23 30 35 40 49 65)]
        [year (range 2011 2018)])
    (let ([url (~a "http://proceedings.mlr.press/v"
                   vl "/index.html")]
          [filename (~a "html/COLT/COLT-" year ".html")])
      (download-html url filename #:exists 'replace)
      (sleep 5)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; AISTATS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (for ([vl (list 2 5 9 15 22 31 33 38 51 54)]
        [year (append (list 2007) (range 2009 2018))])
    (let ([url (~a "http://proceedings.mlr.press/v"
                   vl "/index.html")]
          [filename (~a "html/AISTATS/AISTATS-" year ".html")])
      (download-html url filename #:exists 'replace)
      (sleep 5)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; IJCAI
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (download-html "https://www.ijcai.org/proceedings/2017/"
                 "test.html"
                 #:exists 'replace)

  (for ([year (append (range 1969 1976 2) ;; bi
                      (range 2003 2014 2) ;; bi
                      (range 2015 2018) ;; every year
                      )]
        #:when (= year 2017))
    ;; weired: must have the trailing slash, but only for 2017
    (let ([url (~a "https://www.ijcai.org/proceedings/" year "/")]
          [filename (~a "html/IJCAI/IJCAI-" year ".html")])
      (download-html url filename #:exists 'replace)
      (sleep 5)))

  (for ([year (range 1977 2002 2)])
    (for ([vl (list 1 2)])
      (let ([url (~a "https://www.ijcai.org/proceedings/" year "-" vl)]
            [filename (~a "html/IJCAI/IJCAI-" year ":" vl ".html")])
        (download-html url filename)
        (sleep 5)))) 
  
  )
