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
   diff->sxml
   diff->html
   sxml-diff->html
 )

(import scheme)
(cond-expand
  (chicken-4
   (import chicken)
   (use extras srfi-1 srfi-13 posix files data-structures ports)
   (use salmonella-log-parser salmonella-html-report sxml-transforms))
  ((or chicken-5 chicken-6)
   (import (chicken base)
           (chicken format)
           (chicken pathname)
           (chicken port)
           (chicken string))
   (import salmonella-log-parser
           salmonella-html-report
           srfi-1
           srfi-13
           sxml-transforms))
  (else
   (error "Unsupported CHICKEN version.")))

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
  (let* ((blank '(literal "&nbsp;"))
         (install-ok-1 (count-install-ok log1))
         (install-ok-2 (count-install-ok log2))
         (install-ok-diff (- install-ok-2 install-ok-1))
         (install-fail-1 (count-install-fail log1))
         (install-fail-2 (count-install-fail log2))
         (install-fail-diff (- install-fail-2 install-fail-1))

         (test-ok-1 (count-test-ok log1))
         (test-ok-2 (count-test-ok log2))
         (test-ok-diff (- test-ok-2 test-ok-1))
         (test-fail-1 (count-test-fail log1))
         (test-fail-2 (count-test-fail log2))
         (test-fail-diff (- test-fail-2 test-fail-1))
         (no-test-1 (count-no-test log1))
         (no-test-2 (count-no-test log2))
         (no-test-diff (- no-test-2 no-test-1))

         (doc-1 (count-documented log1))
         (doc-2 (count-documented log2))
         (doc-diff (- doc-2 doc-1))
         (undoc-1 (count-undocumented log1))
         (undoc-2 (count-undocumented log2))
         (undoc-diff (- undoc-2 undoc-1))

         (runtime-1 (total-time log1))
         (runtime-2 (total-time log2))
         (runtime-diff (- runtime-2 runtime-1))

         (eggs-1 (count-total-eggs log1 with-skipped?: #t))
         (eggs-2 (count-total-eggs log2 with-skipped?: #t))
         (eggs-diff (- eggs-2 eggs-1))
         (not-skipped-1 (count-total-eggs log1 with-skipped?: #f))
         (not-skipped-2 (count-total-eggs log2 with-skipped?: #f))
         (not-skipped-diff (- not-skipped-2 not-skipped-1))
         (skipped-1 (length (log-skipped-eggs log1)))
         (skipped-2 (length (log-skipped-eggs log2)))
         (skipped-diff (- skipped-2 skipped-1)))

    (define (colorize good? text)
      `(span (@ (style ,(if good?
                            "color: blue;"
                            "color: red;")))
             ,text))

    (define (good-if-positive n)
      (if (or (boolean? n) (zero? n))
          n
          (colorize (>= n 0) n)))

    (define (good-if-negative n)
      (if (or (boolean? n) (zero? n))
          n
          (colorize (< n 0) n)))

    `((h2 (@ (id "summary")) "Summary")
      (table (@ (border 1))

             ;;;;;;;;;; Header
             (tr
              (th ,blank)
              (th (@ (colspan 2)) "Installation")
              (th (@ (colspan 3)) "Tests")
              (th (@ (colspan 2)) "Documentation")
              (th "Run time")
              (th (@ (colspan 3)) "Total"))

             ;;;;;;;;;; First line
             (tr (@ (class "odd"))
                 (td (i "Log"))
                 ;; Installation
                 (td (i "Ok")) (td (i "Failed"))
                 ;; Tests
                 (td (i "Ok")) (td (i "Failed")) (td (i "No test"))
                 ;; Documentation
                 (td (i "Yes")) (td (i "No"))
                 ;; Runtime
                 (td ,blank)
                 ;; Total
                 (td (i "Eggs")) (td (i "Not skipped")) (td (i "Skipped")))

             ;;;;;;;;;; Second line (log1)
             (tr (@ class "even")
                 (td (b 1))
                 ;; Installation
                 (td ,install-ok-1)
                 (td ,install-fail-1)
                 ;; Tests
                 (td ,test-ok-1)
                 (td ,test-fail-1)
                 (td ,no-test-1)
                 ;; Documentation
                 (td ,doc-1)
                 (td ,undoc-1)
                 ;; Runtime
                 (td ,(prettify-time runtime-1))
                 ;; Total
                 (td ,eggs-1)
                 (td ,not-skipped-1)
                 (td ,skipped-1))

             ;;;;;;;;;; Third line (log2)
             (tr (@ class "even")
                 (td (b 2))
                 ;; Installation
                 (td ,install-ok-2)
                 (td ,install-fail-2)
                 ;; Tests
                 (td ,test-ok-2)
                 (td ,test-fail-2)
                 (td ,no-test-2)
                 ;; Documentation
                 (td ,doc-2)
                 (td ,undoc-2)
                 ;; Runtime
                 (td ,(prettify-time runtime-2))
                 ;; Total
                 (td ,eggs-2)
                 (td ,not-skipped-2)
                 (td ,skipped-2))

             ;;;;;;;;;; Fourth line (diff)
             (tr (@ class "odd")
                 (td (i "Diff"))
                 ;; Installation
                 (td ,(good-if-positive install-ok-diff))
                 (td ,(good-if-negative install-fail-diff))
                 ;; Tests
                 (td ,(good-if-positive test-ok-diff))
                 (td ,(good-if-negative test-fail-diff))
                 (td ,(good-if-negative no-test-diff))
                 ;; Documentation
                 (td ,(good-if-positive doc-diff))
                 (td ,(good-if-negative undoc-diff))
                 ;; Runtime
                 (td ,(colorize (< runtime-diff 0)
                                (prettify-time runtime-diff)))
                 ;; Total
                 (td ,(good-if-positive eggs-diff))
                 (td ,(good-if-positive not-skipped-diff))
                 (td ,(good-if-negative skipped-diff)))
             ))))

(define (render-new/missing-eggs new/missing-eggs log out-dir missing? report-uri1 report-uri2)
  ;; Write html files for installation and test outputs
  (let ((link-mode? (and report-uri1 #t)))
    (unless (or missing? link-mode?)
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

    (zebra-table
     (if missing?
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
                            link-text: (if (status-zero? install-status)
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
                                link-text: (if (status-zero? test-status)
                                               "ok"
                                               (sprintf "fail (status=~a)"
                                                        test-status))
                                report-uri: report-uri2)))))))
          new/missing-eggs))))

(define (render-test-status status)
  (cond ((not status) "")
        ((eq? status -1) "No test")
        (else status)))

(define (diff->sxml log-file-1 log-file-2 out-dir #!key label1 label2 report-uri1 report-uri2)
  ;; Return SXML code representing the diff between log-file-1 and
  ;; log-file-2.  If report-uri1 is #f ("link mode"), will write HTML
  ;; pages for installation and test reports under out-dir.
  (let* ((log1 (read-log-file log-file-1))
         (log2 (read-log-file log-file-2))
         (diff (salmonella-diff log1 log2))
         (diffs (car diff))
         (new/missing-eggs (cadr diff))
         ;; Link mode indicates salmonella-diff will just link to
         ;; reports' pages -- it will not write HTML pages for
         ;; installation and test reports
         (link-mode? (and report-uri1 #t)))
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
                                 (render-test-status status-1)
                                 (render-test-status status-2)
                                 (link-egg-test egg status-1 1 report-uri: report-uri1)
                                 (link-egg-test egg status-2 2 report-uri: report-uri2))))
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
       (pre ,(salmonella-info log2)))))

(define (diff->html log-file-1 log-file-2 out-dir #!key label1 label2 report-uri1 report-uri2)
  (let ((content (page-template
                  (diff->sxml log-file-1 log-file-2 out-dir
                              label1: label1
                              label2: label2
                              report-uri1: report-uri1
                              report-uri2: report-uri2)
                  title: "Salmonella diff")))
    (sxml-diff->html content (make-pathname out-dir "index.html"))))


;;; SXML utils
(define (sxml-diff->html sxml output-file)
  (define (->html)
    (let* ((rules `((literal *preorder* . ,(lambda (t b) b))
                    . ,universal-conversion-rules*)))
      (SRV:send-reply (pre-post-order* sxml rules))))
  (if output-file
      (with-output-to-file output-file ->html)
      (with-output-to-string ->html)))

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
    (if (or (not test-status) (eq? test-status -1))
        ""
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
