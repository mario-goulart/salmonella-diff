(use srfi-1 salmonella-log-parser regex sxml-transforms)

(define-record diff action egg status-1 message-1 status-2 message-2)

(define (salmonella-diff log-file-1 log-file-2)
  (let* ((log1 (read-log-file log-file-1))
         (log2 (read-log-file log-file-2))
         (eggs1 (sort-eggs (log-eggs log1)))
         (eggs2 (sort-eggs (log-eggs log2)))
         (diffs '())
         (add-diff! (lambda (diff)
                      (set! diffs (cons diff diffs)))))
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
                                 (test-message egg log2))))))
     eggs1)
    diffs))


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

(define (diff->html log-file-1 log-file-2 out-dir #!key label1 label2)
  (let ((diffs (salmonella-diff log-file-1 log-file-2)))
    (sxml-diff->html
     (page-template
      `((h1 "Salmonella diff")
        (h2 "Log files")
        (table
         (tr (td 1) (td ,(or label1 log-file-1)))
         (tr (td 2) (td ,(or label2 log-file-2))))
        (h2 "Differences")
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
                   diffs))))
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

(define (link-egg-test egg num)
  (let ((egg (symbol->string egg)))
    `(a (@ (href ,(make-pathname (list (conc "log" num) "test") egg "html")))
        "Test output")))

(define (link-egg-install egg num)
  (let ((egg (symbol->string egg)))
    `(a (@ (href ,(make-pathname (list (conc "log" num) "install") egg "html")))
        "Installation output")))


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
#this [ --out-dir=<dir> ] log1 log2
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
