#lang racket

(require json
         racket/hash
         roman-numeral
         sxml
         "utils.rkt")

(provide acm-gen-bib)

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


(define (acm-new-index)
  ;; get var dataProceedingsJson =, as a json
  (define j (string->jsexpr
             (second (regexp-match #rx"var dataProceedingsJson = (.*)$"
                                   (my/url->string "https://dl.acm.org/proceedings")))))
  ;; returns:
  ;; "https://dl.acm.org/action/doSearch?target=browse-proceedings-specific&ConceptID=118929"

  ;; 1. the widgetId
  ;; 2. pbContext
  (define-values (widgetId pbContext)
    (let ((s (hash-ref (first (first (hash-values (first j))))
                       'ajaxLink)))
      (values (second (regexp-match #rx"widgetId=([^&]*)&" s))
              (second (regexp-match #rx"pbContext=([^&]*)&" s)))))
  ;; 3. a list of ConceptID and conference name
  (apply hash-union
         ;; HACK embed the prefix url into the hash table
         (hash "prefix" (~a "https://dl.acm.org/pb/widgets/proceedings/getProceedings?widgetId="
                            widgetId
                            "&pbContext=" pbContext
                            "&ConceptID="))
         (for/list ([jj j])
           (for/hash ([x (first (hash-values jj))])
             (let ([sp (string-split (hash-ref x 'label) ":")])
               (when (< (length sp) 2)
                 (println sp)
                 (error "length should be 2"))
               (let ([short (first sp)]
                     ;; FIXME I would love to have long, but the data structure won't be
                     ;; as clean
                     [long (string-join (rest sp) ":")]
                     [ConceptID (second (regexp-match #rx"ConceptID=(.*)" (hash-ref x 'ajaxLink)))])
                 (values short ConceptID)))))))

(define (acm-new-conf conf year)
  ;; 1. get a list (year . url)
  (define h (acm-new-index))
  (when (not (hash-has-key? h conf))
    (error "Conf name not valid."))

  (define j (url->json (~a (hash-ref h "prefix") (hash-ref h conf))))
  (define year2link (for/hash ([jj (hash-ref (hash-ref j 'data) 'proceedings)])
                      (values (get-year (hash-ref jj 'title))
                              (~a "https://dl.acm.org" (hash-ref jj 'link)))))
  ;; 2. get the year of interest
  (define proc-url (hash-ref year2link year))

  ;; https://dl.acm.org/doi/proceedings/10.5555/602902
  ;;
  ;; 1. get the number of session
  (define xexp (url->xexp proc-url))
  (define all-headings ((sxpath "//div[contains(@class, 'toc__section')]/a/@id/text()") xexp))
  ;; this is a list of papers
  (define papers
    (apply
     append
     (for/list ([heading all-headings])
       ;; 2. get https://dl.acm.org/doi/proceedings/10.5555/602902?tocHeading=heading30, starting with 1
       (let ([xx (url->xexp (~a proc-url "?tocHeading=" heading))])
         ;; 3. parse the newly opened page for the actual papers
         (let ([p ((sxpath "//div[contains(@class, 'issue-item__content-right')]") xx)])
           (for/list ([pi p])
             (let* ([authors ((sxpath "//ul/li/a/@title/text()") pi)]
                    [title (first ((sxpath "//h5/a/text()") pi))]
                    [link ((sxpath "//a[contains(@data-title, 'PDF')]/@href/text()") pi)]
                    [pdflink (if (empty? link) #f
                                 (string-append "https://dl.acm.org" (first link)))])
               (paper title authors pdflink conf year))))))))
  (string-join (map gen-single-bib papers) "\n"))

(define-syntax-rule (myvoid a ...)
  (void))

(myvoid
 (acm-new-conf "ICCAD" 2003))

(define (acm-conf? conf)
  (hash-has-key? (acm-new-index) conf))

(define (acm-conf-years conf)
  (define h (acm-new-index))
  (when (not (hash-has-key? h conf))
    (error "Conf name not valid."))

  (define j (url->json (~a (hash-ref h "prefix") (hash-ref h conf))))
  (define year2link (for/hash ([jj (hash-ref (hash-ref j 'data) 'proceedings)])
                      (values (get-year (hash-ref jj 'title))
                              (~a "https://dl.acm.org" (hash-ref jj 'link)))))
  ;; 2. get the year of interest
  (sort (hash-keys year2link) <))

(define (acm-gen-bib conf year)
  (case conf
    [(iccad dac isca ispd popl icfp)
     (acm-new-conf (string-upcase (symbol->string conf)) year)]
    #;
    [(pldi dac isca)
     (let ([id (acm-lookup-id conf year)])
       (acm-bib id (string-upcase (symbol->string conf)) year))]
    ;; [(popl) (gen-popl year)]
    ;; [(icfp) (gen-icfp year)]
    [else #f]))

(comment
 (void )

 ;; TODO ISPD?
 (acm-conf? "ISPD")
 (acm-conf-years "ISPD")
 (acm-conf-years "ICCAD")
 (acm-conf-years "DAC")

 (hash-ref h "ICSE")
 (hash-has-key? h "ICCAD")

 (call-with-output-file "a.bib"
   (λ (out)
     (displayln (acm-new-conf "ICCAD" 2008) out))
   #:exists 'replace)

 #f)
