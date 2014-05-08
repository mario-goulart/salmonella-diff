(module salmonella-diff-cmd ()

(import chicken scheme)
(use srfi-1 srfi-13 posix files data-structures ports)
(use salmonella-diff salmonella-html-report)

(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name))))
    (display #<#EOF
#this [ -h | --help ]
#this [ <options> ] log1 log2

<options>:

--out-dir=<dir>
  Directory where to save the HTML diff output

--label1=<label>
  Label for log 1

--label2=<label>
  Label for log 2

--report-uri1=<URI>
  Base URI to the salmonella HTML report for log1 (requires --report-uri2)

--report-uri2=<URI>
  Base URI to the salmonella HTML report for log2 (requires --report-uri1)

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
       (label2 (cmd-line-arg '--label2 args))
       (report-uri1 (cmd-line-arg '--report-uri1 args))
       (report-uri2 (cmd-line-arg '--report-uri2 args)))

  (when (or (member "--help" args)
            (member "-h" args)
            (member "-help" args))
    (usage 0))

  (when (file-exists? out-dir)
    (die out-dir " already exists. Aborting."))

  (when (null? args)
    (usage 1))

  (when (and report-uri1 (not report-uri2))
    (die "--report-uri1 requires --report-uri2 to be provided."))

  (when (and report-uri2 (not report-uri1))
    (die "--report-uri2 requires --report-uri1 to be provided."))

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
      (diff->html log1 log2 out-dir
                  label1: label1
                  label2: label2
                  report-uri1: report-uri1
                  report-uri2: report-uri2))))

) ;; end module
