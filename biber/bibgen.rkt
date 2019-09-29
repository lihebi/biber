#lang racket

(require file/sha1
         net/url
         sxml
         html-parsing
         rackunit
         json
         roman-numeral
         "utils.rkt")

(provide
 gen-bib
 arxiv-bib)


(define (pmlr-bib volume booktitle year)
  ;; PMLR http://proceedings.mlr.press/
  (define pmlr-prefix "http://proceedings.mlr.press/")
  (define xexp (url->xexp (string-append pmlr-prefix "v" (number->string volume))))
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
  (define xexp (url->xexp (string-append thecvf-prefix suffix)))

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

(define (open-review-bib-2020)
  ;; 2020 is on review phase, only one tag is avaiable.
  ;; I'm going to crawl 4 different kinds of bibs:
  ;;
  ;; - submitted: this is used during review session, and should be
  ;;   removed after that.
  ;; - accepted, oral
  ;; - accepted, poster
  ;; - rejected

  ;; https://openreview.net/group?id=ICLR.cc/2020/Conference
  ;; https://openreview.net/group?id=ICLR.cc/2019/Conference
  ;; this seems to give only 1000
  ;; the total number of papers are 2587
  ;; (define iclr-2020-json-url "https://openreview.net/notes?invitation=ICLR.cc%2F2020%2FConference%2F-%2FBlind_Submission&details=replyCount%2Coriginal")
  (define iclr-2020-json-url "https://openreview.net/notes?invitation=ICLR.cc/2020/Conference/-/Blind_Submission")
  ;; total count
  (define count (hash-ref
                 (url->json
                  (string-append iclr-2020-json-url "&offset=0&limit=500"))
                 'count))

  (define iclr-2020-urls (for/list ([i (in-range (ceiling (/ count 500)))])
                           (string-append iclr-2020-json-url
                                          (format "&offset=~a&limit=500" (* i 500)))))
  (define (json->paper j)
    (let* ([number (hash-ref j 'number)]
           [content (hash-ref j 'content)]
           [pdf (hash-ref content 'pdf)]
           [title (hash-ref content 'title)]
           [keywords (hash-ref content 'keywords)])
      (list number pdf title keywords)
      (paper title
             (list (~a "Author" number))
             (string-append "https://openreview.net" pdf)
             "ICLRSubmit"
             2020)))
  (define papers (apply append (for/list ([url iclr-2020-urls])
                                 (for/list ([p (hash-ref (url->json url) 'notes)])
                                   (json->paper p)))))
  (when (= count (length papers))
    (error "Not match count ~a but parsed papers ~a" count (length paper)))
  ;; (gen-single-bib (json->paper (first (hash-ref j 'notes))))
  (string-join (map gen-single-bib papers) "\n"))

(define (gen-iclr year)
  (case year
    [(2020) (open-review-bib-2020)]))

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
  (define xexp (url->xexp url))
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
      ;; (& mdash)
      [(list '& child ...) ""]
      ;; this must be the last list pattern
      [(list name child ...) (string-join (map xexp-get-all-text child) "")]
      [s #:when (string? s) s]
      ;; fallback, probably the default error message is better
      [a (error "No matching")]))

(define (acl-bib conf year)
  (define acl-prefix "https://aclanthology.info/events/")
  (define suffix (string-append (string-downcase conf) "-" (number->string year)))
  (define url (string-append acl-prefix suffix))
  (define xexp (url->xexp url))

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

(define (acm-confid->url id)
  (string-append "https://dl.acm.org/citation.cfm?id="
                 (number->string id) "&preflayout=flat"))

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

(define (acm-bib id conf year)
  ;; (define conf "ICFP")
  ;; (define year 2019)
  ;; (define id 3352468)
  (define xexp (url->xexp (acm-confid->url id)))

  (define paper-xexps (partition-by
                       (λ (x)
                         (not (empty?
                               ((sxpath "//a[contains(@href, 'citation.cfm')]/@href") x))))
                       ((sxpath "//tr/td/span") xexp)))


  (define papers (for/list ([x paper-xexps])
                   (let ([title (xexp-get-all-text (first ((sxpath "//a") (first x))))]
                         [author ((sxpath "//a/text()") (second x))]
                         [pdflink (let ([a ((sxpath
                                             "//a[contains(@href, 'ft_gateway.cfm')]/@href/text()")
                                            x)])
                                    (if (empty? a) #f
                                        (~a "https://dl.acm.org/"
                                            (first
                                             (string-split
                                              (first
                                               a)
                                              "dwn=1"))
                                            "dwn=1"))) ])
                     (paper title author pdflink conf year))))
  (string-join (map gen-single-bib papers) "\n"))

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
  ;; FIXME ??
  (get-year "ASPLOS IX"))


(define (acm-conf-get-proceedings-impl conf
                                       #:skip [skip '()])
  "Return the ID for the specific year of the conf."
  "Return ((year id) ...)"
  ;; FIXME what to do if year appears multiple times
  (define acm-index-url "https://dl.acm.org/proceedings.cfm")
  (define xexp (url->xexp acm-index-url))

  ;; get a list of <strong> for each conference
  ;; get a list of ul AFTER <strong> for year list
  ;; pair them up

  ;; 2499
  (define confs (map xexp-get-all-text
                     ((sxpath "//div[@class='text12']/strong") xexp)))
  (define details ((sxpath "//div[@class='text12']/ul") xexp))

  (when (not (= (length confs)
                (length details)))
    (error "Number does not match"))

  (let ([idx (index-of confs conf)])
    (when idx
      (let ([detail (list-ref details idx)])
        (define ids (map (λ (s) (string->number (first (regexp-match #rx"[0-9]+" s))))
                         ((sxpath "//li/a/@href/text()") detail)))
        (define years (map get-year ((sxpath "//li/a/@title/text()") detail)))
        (when (not (= (length ids)
                      (length years)))
          (error "Number does not match"))
        (let ([res (for/list ([id ids]
                              [year years]
                              #:when (not (member id skip)))
                     (list year id))])
          ;; check for duplication
          ;; TODO this post-condition should be implemented as a function contract?
          (let ([dup (check-duplicates (map first res))])
            (when dup
              (error (format "Error: duplicate: ~a" dup))))
          res)))))

(define (acm-conf-get-proceedings conf)
  (case conf
    [(isca) (acm-conf-get-proceedings-impl "International Symposium on Computer Architecture"
                                           #:skip '(2185870 285930))]
    [(iccad) (acm-conf-get-proceedings-impl "International Conference on Computer-Aided Design")]
    [(dac) (acm-conf-get-proceedings-impl "Design Automation Conference")]
    [(pldi) (acm-conf-get-proceedings-impl "Programming Language Design and Implementation")]))


(define (acm-lookup-id conf year)
  (let ([res (assoc year (acm-conf-get-proceedings conf))])
    (when (not res)
      (error (format "No ~a ~a" conf year)))
    (second res)))

(define (gen-icfp year)
  "17-19"
  (case year
    [(2019) (acm-bib 3352468 "ICFP" year)]
    [(2018) (acm-bib 3243631 "ICFP" year)]
    [(2017) (acm-bib 3136534 "ICFP" year)]))

(define (gen-popl year)
  "2018-19 POPL is not listed in acm proceeding page"
  (case year
    [(2018) (acm-bib 3177123 "POPL" year)]
    [(2019) (acm-bib 3302515 "POPL" year)]))

(define (gen-bib conf year)
  (case conf
    [(pldi dac iccad isca)
     (let ([id (acm-lookup-id conf year)])
       (acm-bib id (string-upcase (symbol->string conf)) year))]
    [(popl) (gen-popl year)]
    [(icml) (gen-icml year)]
    [(aistats) (gen-aistats year)]
    [(cvpr) (gen-cvpr year)]
    [(naacl) (gen-naacl year)]
    [(icfp) (gen-icfp year)]
    [(iclr) (gen-iclr year)]
    ))


(module+ test
  ;; (void (arxiv-bib "cs.AI" 2017 1))

  ;; (void (acm-bib 3352468 "ICFP" 2019))
  ;; (void (gen-isca 2015))

  ;; (acm-lookup-id 'isca 2015)

  (void (acm-bib (acm-lookup-id 'iccad 2015) "Test" 2015))

  ;; (void (acm-bib 2749469 "ISCA" 2015))
  )
