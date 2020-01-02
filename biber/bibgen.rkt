#lang racket

(require file/sha1
         net/url
         sxml
         html-parsing
         rackunit
         json
         "utils.rkt"
         "acm.rkt")

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

(define (open-review-p->paper p decision year)
    (let* ([number (hash-ref p 'number)]
           [content (hash-ref p 'content)]
           [pdf (hash-ref content 'pdf)]
           ;; ("Sirisha Rambhatla" "Xingguo Li" "Jarvis Haupt")
           [authors (hash-ref content 'authors)]
           [title (hash-ref content 'title)]
           [keywords (hash-ref content 'keywords)])
      (paper title
             (case decision
               [(#f) (list (~a "Author" number))]
               [else authors])
             (string-append "https://openreview.net" pdf)
             (case decision
               [(oral) "ICLROral"]
               [(poster) "ICLRPoster"]
               [(reject) "ICLRReject"]
               [(spotlight) "ICLRSpotlight"]
               [(#f) "ICLRSubmit"]
               [else (error "Decision not recognized: " decision)])
             year)))

(define (open-review-json->papers submit-jobj review-jobj year)
  "Parse the json object, and return a list of struct paper.

jobj is the submitted paper database. jobj-decision is optional decision database."
  (case review-jobj
    [(#f) (for/list ([p submit-jobj])
            (open-review-p->paper p #f year))]
    [else
     ;; Steps:
     ;; 1. create hash table for jobj
     (define submit-db (for/hash ([p submit-jobj])
                         (values (hash-ref p 'forum) p)))
     ;; 2. go through different decisions, and generate paper
     (define (get-decision p)
       (let ([dec
              (if (hash-has-key? (hash-ref p 'content) 'recommendation)
                  ;; 2019
                  (hash-ref (hash-ref p 'content) 'recommendation)
                  ;; 2020
                  (hash-ref (hash-ref p 'content) 'decision))])
         (case dec
           [("Reject") 'reject]
           [("Accept (Poster)") 'poster]
           [("Accept (Oral)") 'oral]
           ;; 2020
           [("Accept (Talk)") 'oral]
           ;; 2020
           [("Accept (Spotlight)") 'spotlight]
           [else (error "Decision not recognized:" dec)])))
     (apply append
            ;; three decisions
            (for/list ([decision '(oral spotlight poster reject)])
              (let* ([reviews (filter (λ (p) (eq? (get-decision p) decision)) review-jobj)]
                     [papers (for/list ([r reviews])
                               ;; 3. the forum field is what connects the review and paper
                               (hash-ref submit-db (hash-ref r 'forum)))])
                (for/list ([p papers])
                  (open-review-p->paper p decision year)))))]))

(define (open-review-url->json url)
  (define count (hash-ref
                 (url->json
                  (string-append url "&offset=0&limit=500"))
                 'count))
  (define urls (for/list ([i (in-range (ceiling (/ count 500)))])
                 (string-append url
                                (format "&offset=~a&limit=500" (* i 500)))))
  (apply append (for/list ([url urls])
                  (hash-ref (url->json url) 'notes))))

(define (open-review-bib year)
  ;; (define year 2020)
  (define url-header "https://openreview.net/notes?invitation=")
  ;; TODO withdrawn and desk rejected
  ;; var WITHDRAWN_SUBMISSION_ID = 'ICLR.cc/2020/Conference/-/Withdrawn_Submission';
  ;; var DESK_REJECTED_SUBMISSION_ID = 'ICLR.cc/2020/Conference/-/Desk_Rejected_Submission';
  (define submit-url (case year
                       [(2019) (string-append url-header
                                              "ICLR.cc/2019/Conference/-/Blind_Submission")]
                       [(2020) (string-append url-header
                                              "ICLR.cc/2020/Conference/-/Blind_Submission")]
                       [else (error "Open review does not support year:" year)]))
  (define review-url (case year
                       [(2019) (string-append url-header
                                              "ICLR.cc/2019/Conference/-/Paper.*/Meta_Review")]
                       [(2020) (string-append url-header
                                              "ICLR.cc/2020/Conference/Paper.*/-/Decision")]))

  (define submit-jobj (open-review-url->json submit-url))
  (define review-jobj (if review-url
                          (open-review-url->json review-url)
                          #f))

  ;; (hash-ref (hash-ref (first review-jobj) 'content) 'recommendation)
  (when (and review-jobj
             (not (= (length submit-jobj)
                     (length review-jobj))))
    (error (~a "warning: submit and review not equal: "
               (length submit-jobj) " vs. "
               (length review-jobj))))
  
  (define papers (open-review-json->papers submit-jobj review-jobj year))
  (string-join (map gen-single-bib papers) "\n"))

(define (gen-iclr year)
  (case year
    [(2019 2020) (open-review-bib year)]))

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

(define (gen-bib conf year)
  (case conf
    [(iccad)
     (acm-new-conf (string-upcase (symbol->string conf)) year)]
    #;
    [(pldi dac isca)
     (let ([id (acm-lookup-id conf year)])
       (acm-bib id (string-upcase (symbol->string conf)) year))]
    ;; [(popl) (gen-popl year)]
    [(icml) (gen-icml year)]
    [(aistats) (gen-aistats year)]
    [(cvpr) (gen-cvpr year)]
    [(naacl) (gen-naacl year)]
    ;; [(icfp) (gen-icfp year)]
    [(iclr) (gen-iclr year)]))

