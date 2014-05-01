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
   )

(import chicken scheme)
(use srfi-1 srfi-13 posix files data-structures ports)
(use salmonella-log-parser)

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

) ;; end module
