#lang racket

(require json
         sxml
         "utils.rkt")

(provide open-review-gen-bib)

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

(define (open-review-gen-bib conf year)
  (case conf
    [(iclr) (gen-iclr year)]
    [else #f]))
