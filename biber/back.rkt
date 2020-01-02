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

(module+ test
  ;; (void (arxiv-bib "cs.AI" 2017 1))

  ;; (void (acm-bib 3352468 "ICFP" 2019))
  ;; (void (gen-isca 2015))

  ;; (acm-lookup-id 'isca 2015)

  (void (acm-bib (acm-lookup-id 'iccad 2015) "Test" 2015))

  ;; (void (acm-bib 2749469 "ISCA" 2015))
  )
