
(define library-directories
  '(()
    (slib)
    (chibi)
    (schemacs)
    (schemacs lens)
    (schemacs editor)
    (schemacs elisp-eval)
    ))

(define library-list
  '((slib common)
    (slib filename)
    (slib directory)
    (chibi match)
    (schemacs bitwise)
    (schemacs string)
    (schemacs vector)
    (schemacs hash-table)
    (schemacs lens)
    (schemacs cursor)
    (schemacs lens vector)
    (schemacs pretty)
    (schemacs lens bin-hash-table)
    (schemacs lexer)
    (schemacs editor command)
    (schemacs keymap)
    (schemacs bit-stack)
    (schemacs elisp-eval pretty)
    (schemacs elisp-eval parser)
    (schemacs elisp-eval environment)
    (schemacs elisp-eval format)
    (schemacs editor-impl)
    (schemacs elisp-eval)
    (schemacs elisp-load)
    ))

;;--------------------------------------------------------------------

(define path-separator
  (cond-expand
    ((or cp/m ms-dos msdos freedos os/2 os2
         windows ms-windows reactos react-os
         )
     "\\"
     )
    (else "/")
    ))

(define (library-id->path lib-id suffix)
  (apply
   string-append
   (cons
    "."
    (let loop ((elems lib-id))
      (cond
       ((null? elems) (list suffix))
       ((pair? elems)
        (let*((head (car elems))
              (head
               (cond
                ((integer? head) (number->string head))
                ((symbol? head) (symbol->string head))
                (else (error "not a symbol or integer" head 'in lib-id))
                )))
          (cons path-separator (cons head (loop (cdr elems))))
          )))
      ))))

(cond-expand

  (mit

   (define (loader lib)
     ;;(cf (library-id->path lib ".sld"))
     (load (library-id->path lib ".sld"))
     )

   (for-each loader library-list)

   (disk-save "./schemacs-mitscheme.com")
   (newline)
   (display ";;; Now run the command `mit-scheme --band ./schemacs-mitscheme.com`")
   (newline)
   )

  (guile

   (import (schemacs elisp-eval))

   )

  (gauche

   (define (loader lib)
     (load (library-id->path lib ".sld"))
     )

   (for-each loader library-list)

   )

  (gambit

   (for-each module-search-order-add!
    (map (lambda (lib) (library-id->path lib ".sld"))
         library-directories
         ))

   (define (loader lib)
     (let ((src (library-id->path lib ".sld"))
           (obj (library-id->path lib ""))
           )
       (display "; compile ") (write src) (newline)
       (compile-file src)
       (load obj)
       ))

   (for-each loader library-list)

   )

  (stklos

   (let*((old-lp
          (filter
           (lambda (p) (not (string=? p ".")))
           (load-path)
           ))
         (new-lp
          (let loop
              ((paths
                (map (lambda (lib) (library-id->path lib "/"))
                     library-directories
                     ))
               )
            (cond
             ((null? paths) old-lp)
             (else
              (let ((head (car paths)) (tail (cdr paths)))
                (if (member head old-lp)
                    (loop tail)
                    (cons head (loop tail))
                    )))))
          
          ))
     (display ";; set load-path to:") (newline)
     (for-each (lambda (path) (display "; ") (write path) (newline)) new-lp)
     (load-path new-lp)
     )

   (define (loader lib)
     (let ((src (library-id->path lib ".sld"))
           (obj (library-id->path lib ".ostk"))
           )
       (parameterize ((load-verbose #t))
         (display "; compile ") (write src) (newline)
         (compile-file src obj)
         (load obj)
         )))

   (for-each loader library-list)

   )

  (chibi

   (define (loader lib)
     (load (library-id->path lib ".sld"))
     )

   (for-each loader library-list)

   )

  (else
   (error "this Scheme implementation is not supported")
   ))
