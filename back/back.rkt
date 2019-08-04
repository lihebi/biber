(define (html->pdfs! html)
  "Download pdfs into pdf folder"
  (let* ([confid (path->confid html)]
         [pdf-dir (build-path (pdf-auto-dir) confid)])
    (when (not (directory-exists? pdf-dir))
      (make-directory* pdf-dir))
    (for ([p (parse-acm-proc html)])
      (let ([outf (build-path pdf-dir (~a (gen-id p confid) ".pdf"))]
            [pdflink (third p)])
        (displayln pdflink)
        (when (and pdflink
                   (not (file-exists? outf)))
          (download-pdf pdflink
                        outf))))))

(define (download-test url f)
  (displayln (~a "downloading " f))
  (let* ([in (get-pure-port
              (string->url url)
              #:redirections 8)]
         [out (open-output-file f #:exists 'replace)])
    #;
    (write-bytes
     (port->bytes in)
     out)
    (copy-port in out)
    (close-input-port in)
    (close-output-port out)
    (displayln (~a "Downloaded: " f))))

(module+ test
  (path->confid "./html/2017-ICSE.html")
  
  (html->pdfs! "./html/2017-ICSE.html")

  (download-test
   "https://dl.acm.org/ft_gateway.cfm?id=3098491&ftid=1875094&dwn=1"
   "a.pdf")

  (download-test
   "http://lihebi.com/cv.pdf"
   "a.pdf")
  (download-pdf
   "http://lihebi.com/cv.pdf"
   "a.pdf")
)
