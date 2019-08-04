#lang racket

;; https://dl.acm.org/citation.cfm?id=3097368&preflayout=flat


(require net/url)
(require racket/port)
(require racket/format)
;; needs installing
(require html-parsing)
;; needs installing
(require sxml)
(require roman-numeral)
(require sha)

(require "utils.rkt")

(define (confid->url id)
  (string-append "https://dl.acm.org/citation.cfm?id="
                 id "&preflayout=flat"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file->xexp f)
  (let* ([in (open-input-file f)]
         [res (html->xexp in)])
    (close-input-port in)
    res))
;; (partition-by (λ (x) (eq? x 1)) '(1 2 1 2 1 2 3))

(define (partition-by proc lst)
  "If proc return #t, start a new partition. This particular item is
in new partition"
  (foldl (λ (x acc)
           (if (proc x)
               (append acc (list (list x)))
               (if (empty? acc) acc
                   (append (drop-right acc 1)
                           (list (append (last acc) (list x)))))))
         '() lst))

(define (xexp-a-href xexp)
  (last (first ((sxpath '(a @ href)) `(*TOP* ,xexp)))))

(define (xexp-a-value x)
  (apply
   string-append
   (map (λ (y)
          (cond
            [(and (list? y)
                  (not (eq? (car y) '@)))
             (xexp-a-value y)]
            [(string? y) y]
            [else ""]))
        x)))

(module+ test
  (xexp-a-value '(a
                  (@ (href "citation.cfm?id=263713"))
                  "Does "
                  (& ldquo)
                  "just in time"
                  (& rdquo)
                  " = "
                  (& ldquo)
                  "better late than never"
                  (& rdquo)
                  "?"))
  (xexp-a-value '(a (underline "hello")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACM Index Page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define acm-index-url "https://dl.acm.org/proceedings.cfm")

;; DO NOT print it, it is very large
(define maybe-parse-acm-proc-index
  (let ([res #f])
    (λ ()
      "If not yet done, download acm index, and parse."
      (when (not res)
        (set! res (let* ([index-file (maybe-retrieve-url acm-index-url)]
                         [xexp (file->xexp index-file)])
                    (map (λ (s)
                           (cond
                             [(eq? (car s) 'strong)
                              (last (first ((sxpath '(strong a))
                                            `(*TOP* ,s))))]
                             [(eq? (car s) 'ul)
                              (map (λ (li)
                                     (last li))
                                   ((sxpath '(ul li a @ (*or* title href)))
                                    `(*TOP* ,s)))]))
                         ((sxpath
                           "//div[@class='text12']/child::*"
                           ;; '(html body div div (*or* strong ul))
                           )
                          xexp)))))
      res)))


(module+ test
  (define index-file (maybe-retrieve-url acm-index-url))
  (define xexp (file->xexp index-file))
  (take (maybe-parse-acm-proc-index) 20)
  ((sxpath
    '(html body div div (*or* strong ul)))
   xexp)
  (take ((sxpath
          "//div[@class='text12']/child::*"
          ;; "//div[@class='text12']/ul"
          )
         xexp) 100)
  )

(define (get-proc-names)
  (filter (λ (x) (not (list? x))) (maybe-parse-acm-proc-index)))


(define (get-year str)
  (let ([year (let ([m (regexp-match #rx"[0-9]+" str)])
                (if m
                    (string->number (first m))
                    ;; asplos
                    (if (string-prefix? str "ASPLOS")
                        (list-ref '(82 87 89 91 92 94 96 98 00 02 04 06 08 09 10 11 12)
                                  (sub1 (roman->number (second (string-split str)))))
                        (raise-syntax-error #f (~a "Year not valid: " str)))))])
    (cond
      [(< year 30) (+ year 2000)]
      [(< year 100) (+ year 1900)]
      [else year])))

(module+ test
  (get-year "ICSE '08")
  (get-year "ASPLOS IX"))

(define (get-conf-lst names short
                      #:skip [skip '()]
                      #:rename [rename '()]
                      ;; should be list of lists of two elements,
                      ;; acmid and confid, in string
                      #:add [add '()])
  (let ([lst (apply
              append
              (map (λ (name)
                     (second (member name (maybe-parse-acm-proc-index))))
                   names))])
    (let* ([helper (λ (s) (string-prefix? s "citation"))]
           [titles (filter-not helper lst)]
           [ids (map (λ (s)
                       (first (regexp-match #rx"[0-9]+" s)))
                     (filter helper lst))])
      (sort
       (append
        (for/list ([id ids]
                   [title titles]
                   #:when (not (member id skip)))
          (if (assoc id rename)
              `(,id ,(second (assoc id rename)))
              `(,id ,(~a (number->string (get-year title)) "-" short))))
        add)
       string<? #:key (λ (x) (second x))))))


(define (acm-conf-list conf)
  (case conf
    [(ase) (get-conf-lst '("Automated Software Engineering") "ASE"
                         #:skip '("2358968")
                         #:add '(("2916185" "2015-ASE") ("3155562" "2017-ASE")))]
    [(issta) (get-conf-lst '("International Symposium on Software Testing and Analysis") "ISSTA")]
    [(fse) (get-conf-lst
            '("European Software Engineering Conference / Foundations of Software Engineering"
              "Foundations of Software Engineering") "FSE"
            #:rename '(("1081706" "2005-FSE")
                       ("1595782" "2009-FSED")
                       ("1295014" "2007-FSEC")
                       ("940071" "2003-FSE")
                       ("503209" "2001-FSE")
                       ("318773" "1999-FSE")
                       ("800007" "1983-FSE:1")
                       ("1006147" "1983-FSE:2")))]
    [(icse) (get-conf-lst '("International Conference on Software Engineering") "ICSE"
                          #:rename '(("3098344" "2017-ICSEC")
                                     ("2889160" "2016-ICSEC")
                                     ("2818754" "2015-ICSE:1")
                                     ("2819009" "2015-ICSE:2")
                                     ("2591062" "2014-ICSEC")
                                     ("1806799" "2010-ICSE:1")
                                     ("1810295" "2010-ICSE:2")
                                     ("1585694" "2009-ICSEC")
                                     ("1370175" "2008-ICSEC")
                                     ("1248821" "2007-ICSEC")
                                     ;; future edition ..
                                     ("336512" "2000-ICSEF")))]
    [(msr) (get-conf-lst '("Mining Software Repositories") "MSR")]
    [(paste) (get-conf-lst '("Program Analysis for Software Tools and Engineering") "PASTE")]
    ;; PL
    [(popl) (get-conf-lst '("Principles of Programming Languages") "POPL"
                          #:add '(("3158143" "2018-POPL")))]
    [(pldi) (get-conf-lst '("Programming Language Design and Implementation") "PLDI")]
    [(onward) (get-conf-lst '("New Ideas, New Paradigms, and Reflections on Programming and Software") "Onward!")]
    [(oopsla) (get-conf-lst '("Object-Oriented Programming Systems, Languages, and Applications") "OOPSLA")]
    [(cgo) (get-conf-lst '("Code Generation and Optimization") "CGO")]
    [(haskell) (get-conf-lst '("Haskell") "Haskell")]
    [(icfp) (get-conf-lst
             ;; typo ...
             '("International Conference on Functional Programmuing") "ICFP"
             #:skip '("3161598")
             #:add '(("3136534" "2017-ICFP")))]
    [(lfp) (get-conf-lst '("LISP and Functional Programming") "LFP")]
    [(sigplan) (get-conf-lst '("SIGPLAN Symposium") "SIGPLAN"
                             ;; no pdf
                             #:skip '("800004"))]
    [(asplos) (get-conf-lst '("Architectural Support for Programming Languages and Operating Systems") "ASPLOS")]
    ;; OS
    [(osdi) (get-conf-lst '("Operating Systems Design and Implementation") "OSDI")]
    [(sosp) (get-conf-lst '("Symposium on Operating Systems Principles") "SOSP"
                          ;; 2015 SOSP history day
                          #:skip '("2830903"))]
    ;; other
    [(kdd) (get-conf-lst '("Knowledge Discovery and Data Mining") "KDD"
                         #:skip '("312179" "349093" "502786"))]
    [(stoc) (get-conf-lst '("Symposium on Theory of Computing") "STOC")]
    [(vldb) (get-conf-lst '("Very Large Data Bases") "VLDB")]
    [(icml) (get-conf-lst '("International Conference on Machine Learning") "ICML")]
    [else (error (~a "Conf not supported: " conf))]))


(module+ test
  ;; this takes 1 sec
  (get-proc-names)
  
  (check-duplicates (map second (acm-conf-list 'ase)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; proceeding page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (acm-proc->papers confid)
  (let ([xexp (file->xexp (maybe-retrieve-url (confid->url confid)))])
    (let ([papers (partition-by
                   (λ (x)
                     (string-prefix? (first x) "citation"))
                   (filter-not
                    (λ (lst)
                      (string-prefix? (first lst) "https"))
                    (map (λ (x)
                           (list (xexp-a-href x)
                                 (xexp-a-value x)))
                         ((sxpath '(body div div div table tr td span a)) xexp))))])
      papers)))

(define (confid->paper-meta confid)
  "Return a list of papers: ((title authors pdflink) ...)"
  (let ([papers (acm-proc->papers confid)])
    (let ([helper (λ (x pref)
                    (for/list ([item x]
                               #:when (string-prefix? (first item) pref))
                      (second item)))])
      (let ([get-authors (λ (p)
                           (helper p "author"))]
            [get-title (λ (p)
                         (let ([lst (helper p "citation")])
                           (if (empty? lst) #f
                               (let ([res (first lst)])
                                 (if (string? res) res #f)))))]
            [get-pdflink (λ (p)
                           (let ([lst (for/list ([item p]
                                                 #:when (string-prefix? (first item) "ft_gateway"))
                                        (let ([orig (first item)])
                                          (~a "https://dl.acm.org/"
                                              (first
                                               (string-split orig "dwn=1"))
                                              "dwn=1")))])
                             (if (empty? lst) #f (first lst))))])
        (filter
         (λ (x)
           (and
            ;; has authors
            (not (empty? (second x)))
            ;; has title
            (first x)))
         (map (λ (p)
                (list (get-title p)
                      (get-authors p)
                      (get-pdflink p)))
              papers))))))

(define (acm-conf->bib confid pref)
  (~a "\n"
      ;; an empty line before, because it seems that the first
      ;; one is not recognized
      (string-join
       (for/list ([meta (confid->paper-meta confid)])
         (let ([title (first meta)]
               [authors (second meta)]
               [pdflink (third meta)])
           (paper->bib title authors pdflink pref))) "\n")))



(define bib-dist-dir (make-parameter "/home/hebi/github/biber-dist/"))

(define (write-file str filename)
  (displayln (~a "Writing to " filename))
  (when (not (file-exists? filename))
      (let ([out (open-output-file filename #:exists 'replace)])
        (write-string str out)
        (close-output-port out))))


(define (acm-genbib-conf conf)
  (for/list ([conf (acm-conf-list conf)])
    (let* ([confid (first conf)]
           [pref (second conf)]
           [bibfile (build-path (bib-dist-dir) (pref->bibpath pref))])
      (when (not (file-exists? bibfile))
        (write-file (acm-conf->bib confid pref)
                    bibfile)))))

(module+ test


  ;; all in all, I just need to run this
  (for [(conf '(icfp pldi popl cgo osdi sosp))]
    (acm-genbib-conf conf))
  
  (acm-conf->bib "3192366" "2018-PLDI")

  (build-path (bib-dist-dir) (pref->bibpath  "2018-PLDI"))
  )

;; (define (path->confid p)
;;   (~a (path->year p) "-"
;;       (path->conf p)))


