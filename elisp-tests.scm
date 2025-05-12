(import
  (scheme base)
  (scheme case-lambda)
  (scheme write)
  (only (schemacs elisp-eval environment)
        elisp-eval-error-type?
        write-elisp-eval-error
        )
  (only (schemacs elisp-eval)
        elisp-load!
        elisp-eval!
        )
  )


(define elisp-ert-dependencies
  ;; This list of programs defines all of the dependencies of the
  ;; Emacs package "ERT" (Emacs Regression Tests). These are loaded
  ;; explicitly here until we get the "require" macro working
  ;; correctly.
  ;;------------------------------------------------------------------
  '("./elisp/lisp/subr.el"
    "./elisp/lisp/emacs-lisp/cl-lib.el"
    "./elisp/lisp/emacs-lisp/debug.el"
    "./elisp/lisp/emacs-lisp/backtrace.el"
    "./elisp/lisp/emacs-lisp/ewoc.el"
    "./elisp/lisp/emacs-lisp/find-func.el"
    "./elisp/lisp/emacs-lisp/pp.el"
    "./elisp/lisp/emacs-lisp/map.el"
    "./elisp/lisp/emacs-lisp/ert.el"
    ))

(define load-deps
  (case-lambda
    (() (load-deps elisp-ert-dependencies))
    ((files)
     (let loop ((files files) (count 0))
       (cond
        ((null? files)
         (display "; successfully loaded ")
         (write count)
         (display " files.\n")
         #t)
        (else
         (let*((file (car files))
               (result
                (let ()
                  (display "; load ")
                  (write file)
                  (newline)
                  (elisp-load! file)
                  )))
           (cond
            ((or (not result)
                 (error-object? result)
                 (elisp-eval-error-type? result)
                 )
             (display "; halted after loading ")
             (write count)
             (display " files.\n")
             (display "; error while loading ")
             (write file)
             (newline)
             (when result
               (write-elisp-eval-error result)
               (newline)
               )
             #f)
            (else
             (loop (cdr files) (+ 1 count)))))
         ))))))


;;(load-deps '("./elisp/test/lisp/emacs-lisp/cl-lib-tests.el"))

(when (load-deps)
  (display "; running test \"ert-run-tests-batch-and-exit\"\n")
  (let ((result
         (elisp-eval!
          '(ert-run-tests-batch-and-exit
            '(not (or (tag :expensive-test)
                      (tag :unstable)
                      (tag :nativecomp)))))))
    (cond
     ((or (error-object? result)
          (elisp-eval-error-type? result)
          )
      (write-elisp-eval-error result)
      #f)
     (else result)
     )))
