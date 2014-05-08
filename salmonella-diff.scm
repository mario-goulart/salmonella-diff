(module salmonella-diff
  (salmonella-diff

   ;; diff record
   make-diff
   diff-action
   diff-egg
   diff-status-1
   diff-message-1
   diff-status-2
   diff-message-2

   ;; new/missing-egg record
   make-new/missing-egg
   new/missing-egg-egg
   new/missing-egg-status
   new/missing-egg-install-status
   new/missing-egg-install-message
   new/missing-egg-test-status
   new/missing-egg-test-message

   ;; procedures
   diff->html
)

(import chicken scheme)
(use srfi-1 srfi-13 posix files data-structures ports)
(use salmonella-log-parser salmonella-html-report sxml-transforms)

(define-record diff action egg status-1 message-1 status-2 message-2)

;; status: new or missing
(define-record new/missing-egg status egg install-status install-message test-status test-message)

(define (salmonella-diff log1 log2)
  ;; Returns a list (<diffs> <new/missing eggs>
  (let* ((eggs1 (sort-eggs (log-eggs log1)))
         (eggs2 (sort-eggs (log-eggs log2)))
         (diffs '())
         (add-diff! (lambda (diff)
                      (set! diffs (cons diff diffs))))
         (new/missing '())
         (add-new/missing! (lambda (new/missing-egg)
                             (set! new/missing
                                   (cons new/missing-egg new/missing)))))
    (for-each
     (lambda (egg)
       (let ((install-status-1 (install-status egg log1))
             (install-status-2 (install-status egg log2))
             (egg-version-1 (egg-version egg log1))
             (egg-version-2 (egg-version egg log2))
             (test-status-1 (test-status egg log1))
             (test-status-2 (test-status egg log2)))
         (unless (eq? install-status-1 install-status-2)
           (add-diff! (make-diff 'install
                                 egg
                                 install-status-1
                                 (install-message egg log1)
                                 install-status-2
                                 (install-message egg log2))))
         (unless (equal? egg-version-1 egg-version-2)
           (add-diff! (make-diff 'version
                                 egg
                                 egg-version-1
                                 ""
                                 egg-version-2
                                 "")))
         (unless (eq? test-status-1 test-status-2)
           (add-diff! (make-diff 'test
                                 egg
                                 test-status-1
                                 (test-message egg log1)
                                 test-status-2
                                 (test-message egg log2))))
         (unless (memq egg eggs2)
           (add-new/missing! (make-new/missing-egg
                              'missing
                              egg
                              install-status-1
                              (install-message egg log1)
                              test-status-1
                              (test-message egg log1))))))
     eggs1)

    ;; Find eggs that disappeared
    (for-each
     (lambda (egg)
       (unless (memq egg eggs1)
         (add-new/missing! (make-new/missing-egg
                            'new
                            egg
                            (install-status egg log2)
                            (install-message egg log2)
                            (test-status egg log2)
                            (test-message egg log2)))))
     eggs2)

    (list diffs new/missing)))

(define (write-action-report! egg action log lognum out-dir)
  (let ((content
         (page-template
          ((case action
             ((install) egg-installation-report)
             ((test) egg-test-report)
             (else (error 'write-action-report! "invalid action" action)))
           egg log)
          title: (conc egg ": "
                       (case action
                         ((install) "installation")
                         ((test) "test"))
                       " report")))
        (output-file
         (make-pathname (list out-dir
                              (conc "log" lognum)
                              (symbol->string action))
                        (symbol->string egg)
                        "html")))
    (sxml-diff->html content output-file)))

(define (render-summary log1 log2)
  (let ((blank '(literal "&nbsp;")))
    `((h2 (@ (id "summary")) "Summary")
      (table
       (tr (th "Installation")
           (th "Tests")
           (th "Documentation")
           (th "Run time")
           (th "Total"))
       (tr
        ;; Installation
        (td ,(zebra-table
              '("" 1 2)
              `(("Ok"
                 ,(count-install-ok log1)
                 ,(count-install-ok log2))
                ("Failed"
                 ,(count-install-fail log1)
                 ,(count-install-fail log2))
                (,blank ,blank ,blank))))

        ;; Tests
        (td ,(zebra-table
              '("" 1 2)
              `(("Ok"
                 ,(count-test-ok log1)
                 ,(count-test-ok log2))
                ("Failed"
                 ,(count-test-fail log1)
                 ,(count-test-fail log2))
                ("No test"
                 ,(count-no-test log1)
                 ,(count-no-test log2)))))

        ;; Documentation
        (td ,(zebra-table
              '("" 1 2)
              `(("Documented"
                 ,(count-documented log1)
                 ,(count-documented log2))
                ("Undocumented"
                 ,(count-undocumented log1)
                 ,(count-undocumented log2))
                (,blank ,blank ,blank))))

        ;; Run time
        (td ,(zebra-table
              '(1 2)
              `((,(prettify-time (inexact->exact (total-time log1)))
                 ,(prettify-time (inexact->exact (total-time log2))))
                (,blank ,blank)
                (,blank ,blank))))


        ;; Total
        (td ,(zebra-table
              '("" 1 2)
              `(("Total number of eggs"
                 ,(count-total-eggs log1 with-skipped?: #t)
                 ,(count-total-eggs log2 with-skipped?: #t))
                ("Not skipped"
                 ,(count-total-eggs log1 with-skipped?: #f)
                 ,(count-total-eggs log2 with-skipped?: #f))
                ("Skipped"
                 ,(length (log-skipped-eggs log1))
                 ,(length (log-skipped-eggs log2))))))
        )))))


(define (render-new/missing-eggs new/missing-eggs log out-dir missing? report-uri1 report-uri2)
  ;; Write html files for installation and test outputs
  (unless missing?
    (for-each
     (lambda (n/m)
       (let* ((egg (new/missing-egg-egg n/m))
              (test-status (new/missing-egg-test-status n/m))
              (test-message (new/missing-egg-test-message n/m))
              (install-status (new/missing-egg-install-status n/m))
              (install-message (new/missing-egg-install-message n/m)))
         (write-action-report! egg 'install log 2 out-dir)
         (write-action-report! egg 'test log 2 out-dir)))
     new/missing-eggs))

  (zebra-table (if missing?
                   '("Egg")
                   '("Egg" "Install status" "Test status"))
               (map (lambda (n/m)
                      (let ((egg (new/missing-egg-egg n/m))
                            (test-status (new/missing-egg-test-status n/m))
                            (install-status (new/missing-egg-install-status n/m)))
                        (if missing?
                            (list (new/missing-egg-egg n/m))
                            (list (new/missing-egg-egg n/m)
                                  `(,(link-egg-install
                                      egg
                                      2
                                      link-text: (if (and install-status
                                                          (zero? install-status))
                                                     "ok"
                                                     (sprintf "fail (status=~a)"
                                                              install-status))
                                      report-uri: report-uri2))
                                  (if (or (not test-status) (eq? test-status -1))
                                      "No test"
                                      `(,(link-egg-test
                                          egg
                                          test-status
                                          2
                                          link-text: (if (zero? test-status)
                                                         "ok"
                                                         (sprintf "fail (status=~a)"
                                                                  test-status))
                                          report-uri: report-uri2)))))))
                    new/missing-eggs)))

(define (diff->html log-file-1 log-file-2 out-dir #!key label1 label2 report-uri1 report-uri2)
  (let* ((log1 (read-log-file log-file-1))
         (log2 (read-log-file log-file-2))
         (diff (salmonella-diff log1 log2))
         (diffs (car diff))
         (new/missing-eggs (cadr diff))
         ;; Link mode indicates salmonella-diff will just link to
         ;; reports' pages -- it will not write HTML pages for install
         ;; an tests
         (link-mode? (and report-uri1 #t)))
    (sxml-diff->html
     (page-template
      `((h1 "Salmonella diff")
        (h2 "Log files")
        (table
         (tr (td 1) (td ,(or label1 log-file-1)))
         (tr (td 2) (td ,(or label2 log-file-2))))
        ,(render-summary log1 log2)
        (h2 "Differences detailed")
        ,(if (null? diffs)
             '(p "No differences")
             (zebra-table
              '("Egg" "Phase" "Status 1" "Status 2" "Output 1" "Output 2")
              (map (lambda (d)
                     (let ((egg (diff-egg d))
                           (action (diff-action d)))
                       (when (and (memq action '(install test))
                                  (not link-mode?))
                         (write-action-report! egg action log1 1 out-dir)
                         (write-action-report! egg action log2 2 out-dir))
                       (case action
                         ((install)
                          (list egg
                                "Installation"
                                (diff-status-1 d)
                                (diff-status-2 d)
                                (link-egg-install egg 1 report-uri: report-uri1)
                                (link-egg-install egg 2 report-uri: report-uri2)))
                         ((test)
                          (let ((status-1 (diff-status-1 d))
                                (status-2 (diff-status-2 d)))
                            (list egg
                                  "Test"
                                  (if (eq? status-1 -1) "No test" status-1)
                                  (if (eq? status-2 -1) "No test" status-2)
                                  (link-egg-test egg status-1 1 report-uri: report-uri1)
                                  (link-egg-test egg status-2 2 report-uri: report-uri1))))
                         ((version)
                          (list egg
                                "Version check"
                                (diff-status-1 d)
                                (diff-status-2 d)
                                ""
                                "")))))
                   diffs)))
        ,(let ((new-eggs (filter (lambda (n/m)
                                   (eq? 'new (new/missing-egg-status n/m)))
                                 new/missing-eggs))
               (missing-eggs (filter (lambda (n/m)
                                       (eq? 'missing (new/missing-egg-status n/m)))
                                     new/missing-eggs)))
           (cond ((and (null? new-eggs) (null? missing-eggs))
                  '())
                 ((null? new-eggs)
                  `((h2 (@ (id "missing-eggs")) "Missing eggs")
                    ,(render-new/missing-eggs missing-eggs log1 out-dir #t report-uri1 report-uri2)))
                 (else
                  `((h2 (@ (id "new-eggs")) "New eggs")
                    ,(render-new/missing-eggs new-eggs log2 out-dir #f report-uri1 report-uri2)))))

        (h2 (@ (id "environment-information")) "Environments information")
        (h3 (@ (id "env1")) "Environment 1")
        (pre ,(salmonella-info log1))
        (h3 (@ (id "env2")) "Environment 2")
        (pre ,(salmonella-info log2)))
      title: "Salmonella diff")
     (make-pathname out-dir "index.html"))))


;;; SXML utils
(define (sxml-diff->html sxml output-file)
  (with-output-to-file output-file
    (lambda ()
      (let* ((rules `((literal *preorder* . ,(lambda (t b) b))
                      . ,universal-conversion-rules*)))
      (SRV:send-reply (pre-post-order* sxml rules))))))

(define (page-template content #!key title)
  `((literal
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
     "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\">")
    (html (@ (xmlns "http://www.w3.org/1999/xhtml"))
          (head
           (meta (@ (charset "utf-8")))
           (title ,title)
           (link (@ (rel "stylesheet")
                    (href ,(salmonella-page-css))
                    (type "text/css"))))
          (body
           (div (@ (id "content"))
                ,content)))))

(define (link-egg-test egg test-status num #!key link-text report-uri)
  (let ((egg (symbol->string egg)))
    (if (eq? test-status -1)
        "No test"
        `(a (@ (href ,(if report-uri
                          (uri-append report-uri (make-pathname "test" egg "html"))
                          (make-pathname (list (conc "log" num) "test") egg "html"))))
            ,(or link-text "Test output")))))

(define (link-egg-install egg num #!key link-text report-uri)
  (let ((egg (symbol->string egg)))
    `(a (@ (href ,(if report-uri
                      (uri-append report-uri (make-pathname "install" egg "html"))
                      (make-pathname (list (conc "log" num) "install") egg "html"))))
        ,(or link-text "Installation output"))))


;;; Utils
(define (uri-append uri path)
  (if (string-null? path)
      uri
      (let ((uri-no-/ (string-chomp uri "/"))
            (path-no-/ (if (eq? (string-ref path 0) #\/)
                           (substring path 1)
                           path)))
        (string-append uri-no-/ "/" path-no-/))))

) ;; end module
