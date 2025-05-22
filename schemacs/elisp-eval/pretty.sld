(define-library (schemacs elisp-eval pretty)
  ;; This library defines an extension to `(schemacs pretty)`
  ;; with rules specific to pretty-printing Emacs Lisp.
  (import
    (scheme base)
    (scheme write)
    (scheme case-lambda)
    (only (schemacs pretty)
          pretty   print   qstr   pp-type?
          indent-by   newline-indent   line-break
          bracketed   form   join-by   join-lines
          )
    )
  (import (only (schemacs match) match))
  (export elisp-pretty elisp-print)

  (begin

    (define elisp-pretty
      (case-lambda
        ((form) (pretty (print (pp form) (line-break))))
        ((port form) (pretty port (print (pp form) (line-break))))
        ))


    (define (pp-form func args) (apply form 1 func args))


    (define (pp-quoted char form) (print char (indent-by 1 (pp form))))


    (define (pp form)
      (match form
        (() "nil")
        (('progn body ...) (block-form "progn" 0 2 2 body))
        (('prog1 body ...) (block-form "prog1" 1 6 2 body))
        (('prog2 body ...) (block-form "prog2" 2 6 2 body))
        (('let  defs body ...) (pp-let "let " defs body))
        (('let* defs body ...) (pp-let "let*" defs body))
        (('cond conditions ...) (pp-cond conditions))
        (('while condition body ...) (block-form "while" 1 7 2 body))
        (('until condition body ...) (block-form "until" 1 7 2 body))
        (('defun name args body ...) (pp-defun "defun" name args body))
        (('defmacro name args body ...) (pp-defun "defmacro" name args body))
        (('declare (forms ...)) (pp-declare forms))
        (('|`| form)   (pp-quoted #\` form))
        (('|,| form)   (pp-quoted #\, form))
        (('quote form) (pp-quoted #\' form))
        ;;------------------------------------------
        ((func args ...) (pp-form func args))
        (any
         (cond
          ((pp-type? any) any)
          ((string? any) (print (qstr any)))
          (else (print any))
          ))
        ))


    (define elisp-print pp)


    (define (block-form sym num-init-args init-indent indent body)
      ;; This is used for things like "let", "cond", and "progn", where
      ;; basically each element of the body needs to be printed on it's
      ;; own line. You can choose some number of initial elements with
      ;; their own indentation for things like "let" and "prog1" where the
      ;; first elements of the form have separate indentation from the
      ;; rest of the body.
      (define (take i keep body)
        (cond
         ((and (< i num-init-args) (pair? body))
          (take (+ 1 i) (cons (car body) keep) (cdr body)))
         (else (values (reverse keep) body))
         ))
      (let-values (((inits body) (take 0 '() body)))
        (cond
         ((pair? body)
          (bracketed 2 #\( #\) sym
           (cond
            ((null? inits) #f)
            ((pair? inits) 
             (indent-by init-indent #\space
              (apply join-lines (map pp inits))))
            (else (error "not a list" inits)))
           (line-break)
           (apply join-lines (map pp body))))
         (else (apply form sym inits))
         )))


    (define (pp-let sym defs body)
      (apply bracketed 2 #\( #\) sym
       (indent-by 4 (apply join-lines (map pp-def defs)))
       (map pp body)))


    (define (pp-def def)
      (match def
        (((? symbol? sym) sym forms ...)
         (form sym
          (indent-by
           (+ 1 (string-length (symbol->string sym)))
           (if (<= (length forms) 1)
               (pp (car forms))
               (apply join-lines (map pp forms)))
           (line-break)
           )))
        (,any (pp any))
        ))


    (define (pp-cond conditions)
      (define (pp-branch condition)
        (match condition
          ((condition body ...)
           (form 1 (pp condition) (newline-indent) (apply join-lines body))
           )))
      (bracketed
       1 #\( #\)
       (apply join-lines "cond" (map pp-branch conditions))))


    (define (pp-defun sym name args body)
      (form 2 (print sym) (print name) (print args)
       (newline-indent) (apply join-lines (map pp body))))


    (define (pp-declare forms)
      (define (pp-decl forms)
        (match forms
          (() (print "()"))
          ((func args ...) (pp-form func args))
          (any (print any))
          ))
      (bracketed
       4 #\( #\) "declare" (newline-indent)
       (apply join-lines (map pp-decl forms)))
      )

    ))
