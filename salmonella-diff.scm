(use srfi-1 salmonella-log-parser regex sxml-transforms)

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


;;; Diff -> HTML
(define (write-html egg action message num out-dir)
  (sxml-diff->html
   (page-template
    (list (case action
            ((install) `(h1 "Installation output for " ,egg))
            ((test) `(h1 "Test output for " ,egg)))
          `(pre ,message))
    title: (conc "Test output (" num ") for " egg))
   (make-pathname (list out-dir
                        (conc "log" num)
                        (symbol->string action))
                  (symbol->string egg)
                  "html")))


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


(define (render-new/missing-eggs new/missing-eggs out-dir missing?)
  ;; Write html files for installation and test outputs
  (unless missing?
    (for-each
     (lambda (n/m)
       (let* ((egg (new/missing-egg-egg n/m))
              (test-status (new/missing-egg-test-status n/m))
              (test-message (new/missing-egg-test-message n/m))
              (install-status (new/missing-egg-install-status n/m))
              (install-message (new/missing-egg-install-message n/m)))
         (write-html egg 'install install-message 2 out-dir)
         (write-html egg 'test test-message 2 out-dir)))
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
                                      (if (and install-status (zero? install-status))
                                          "ok"
                                          (sprintf "fail (status=~a)" install-status))))
                                  (if (or (not test-status) (eq? test-status -1))
                                      "No test"
                                      `(,(link-egg-test
                                          egg
                                          2
                                          (if (zero? test-status)
                                              "ok"
                                              (sprintf "fail (status=~a)" test-status)))))))))
                    new/missing-eggs)))


(define (diff->html log-file-1 log-file-2 out-dir #!key label1 label2)
  (let* ((log1 (read-log-file log-file-1))
         (log2 (read-log-file log-file-2))
         (diff (salmonella-diff log1 log2))
         (diffs (car diff))
         (new/missing-eggs (cadr diff)))
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
                       (when (memq action '(install test))
                         (write-html egg action (diff-message-1 d) 1 out-dir)
                         (write-html egg action (diff-message-2 d) 2 out-dir))
                       (case action
                         ((install)
                          (list egg
                                "Installation"
                                (diff-status-1 d)
                                (diff-status-2 d)
                                (link-egg-install egg 1)
                                (link-egg-install egg 2)))
                         ((test)
                          (list egg
                                "Test"
                                (diff-status-1 d)
                                (diff-status-2 d)
                                (link-egg-test egg 1)
                                (link-egg-test egg 2)))
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
                    ,(render-new/missing-eggs missing-eggs out-dir #t)))
                 (else
                  `((h2 (@ (id "new-eggs")) "New eggs")
                    ,(render-new/missing-eggs new-eggs out-dir #f)))))

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

(define page-css "http://wiki.call-cc.org/chicken.css")

(define (page-template content #!key title)
  `((literal
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
     "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\">")
    (html (@ (xmlns "http://www.w3.org/1999/xhtml"))
          (head
           (meta (@ (charset "utf-8")))
           (title ,title)
           (link (@ (rel "stylesheet")
                    (href ,page-css)
                    (type "text/css"))))
          (body
           (div (@ (id "content"))
                ,content)))))


(define (zebra-table header rows)
  `(table
    (tr ,@(map (lambda (h) `(th ,h)) header))
    ,(let ((odd-row #f))
       (map (lambda (row)
              (set! odd-row (not odd-row))
              `(tr (@ (class ,(if odd-row "odd" "even")))
                   ,@(map (lambda (cell) `(td ,cell)) row)))
            rows))))

(define (link-egg-test egg num #!optional link-text)
  (let ((egg (symbol->string egg)))
    `(a (@ (href ,(make-pathname (list (conc "log" num) "test") egg "html")))
        ,(or link-text "Test output"))))

(define (link-egg-install egg num #!optional link-text)
  (let ((egg (symbol->string egg)))
    `(a (@ (href ,(make-pathname (list (conc "log" num) "install") egg "html")))
        ,(or link-text "Installation output"))))


;;; Utils
(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (cut string-match (conc option "=(.*)") <>) args)))
    (and val (cadr val))))


(define (die . msg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print (apply conc msg))))
  (exit 1))


(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name))))
    (display #<#EOF
#this [ -h | --help ]
#this [ --out-dir=<dir> ] [ --label1=<label1> ] [ --label2=<label2> ] log1 log2
EOF
)
    (newline)
    (when exit-code (exit exit-code))))


;;;
;;; Command line parsing
;;;
(let* ((args (command-line-arguments))
       (out-dir (or (cmd-line-arg '--out-dir args) "salmonella-diff-html"))
       (label1 (cmd-line-arg '--label1 args))
       (label2 (cmd-line-arg '--label2 args)))

  (when (or (member "--help" args)
            (member "-h" args)
            (member "-help" args))
    (usage 0))

  (when (file-exists? out-dir)
    (die out-dir " already exists. Aborting."))

  (when (null? args)
    (usage 1))

  (let ((logs (remove (lambda (arg)
                        (string-prefix? "--" arg))
                      args)))
    (unless (= (length logs) 2)
      (usage 1))

    (create-directory out-dir 'with-parents)
    (create-directory (make-pathname (list out-dir "log1") "install") 'with-parents)
    (create-directory (make-pathname (list out-dir "log1") "test") 'with-parents)
    (create-directory (make-pathname (list out-dir "log2") "install") 'with-parents)
    (create-directory (make-pathname (list out-dir "log2") "test") 'with-parents)

    (let ((log1 (car logs))
          (log2 (cadr logs)))
      (diff->html log1 log2 out-dir label1: label1 label2: label2))))
