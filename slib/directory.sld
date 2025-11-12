;;; "dirs.scm" Directories.
; Copyright 1998, 2002 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; Packaged for R7RS Scheme by Peter Lane, 2017
;;
;; Added pathname->dirname as a new function to replace pathname->vicinity
;; -- this removes the need for SRFI 59

;; Modified for use in the "Schemacs" project
;; by Ramin Honary, copyright 2025.
;;
;; This version of the "(SLIB DIRECTORY)" source code with
;; modifications by Ramin Honary are also subject to the terms of the
;; GNU General Public License (version 3 or later) which grants
;; license to users of this source code with additional conditions
;; specified therein, while respecting the rights of the original
;; authors of this source code by including the original copyright
;; statement written at the top of this file. A copy of the GNU GPL
;; license document is included in the top-level directory of this
;; project.
;;--------------------------------------------------------------------

(define-library (slib directory)
  (export current-directory
          directory-for-each
          directory*-for-each
          make-directory
          pathname->dirname ;; New function added to remove need for SRFI 59
          )
  (import (scheme base)
          (scheme case-lambda)
          (slib common)
          (slib filename))

  (cond-expand
    (guile
     (import
       (only (guile)
             mkdir dirname getcwd opendir readdir closedir)))

    (gauche
     (import
       (file util)))

    (stklos
     (import (srfi 170))
     (begin
       (define dirname dirname)
       (define directory-files directory-files)
       )
     )

    ((library (chibi filesystem))
     (import (chibi filesystem)
             (chibi pathname))
     (begin ; current-directory exported
       (define make-directory create-directory*)
       (define (pathname->dirname path)
         (string-append (path-directory path) "/"))
       (define list-directory-files directory-files)))

    (else)
    )

  (begin
    ;; functions must be defined in platform specific ways

    (cond-expand
      (guile
       (begin
         (define current-directory getcwd)
         (define make-directory mkdir)
         (define opendir opendir)
         (define closedir closedir)
         (define readdir readdir)
         ))

      (gauche
        (begin
          ; current-directory exported
          (define (make-directory str) (current-directory str))
          (define (pathname->dirname path)
            (let-values (((dir name ext) (decompose-path path)))
                        dir))
          (define list-directory-files directory-list)))

      (kawa
        (import (only (kawa lib files) create-directory path-directory)
                (only (kawa lib ports) current-path)
                (only (kawa base) as invoke))
        (begin
          (define current-directory current-path)
          (define make-directory create-directory)
          (define (pathname->dirname path)
            (let* ((dir (path-directory path))
                   (chars (reverse (string->list dir))))
              (if (and (not (null? chars))
                       (char=? #\. (car chars))) ; Kawa sometimes adds a 'dot' to end, so remove it
                (list->string (reverse (cdr chars)))
                dir)))
          (define (list-directory-files dir)
            (map (lambda (file) ; list-directory-files must return just the filenames
                   (let ((path (invoke file 'toString)))
                     (string-copy path (string-length (pathname->dirname path)))))
                 (invoke (java.io.File (as String dir)) 'listFiles)))))

      (larceny
        (import (primitives current-directory list-directory)
                (only (srfi 59) pathname->vicinity))
        (begin
          ; current-directory exported
          (define (make-directory str) (system (string-append "mkdir " str)))
          (define pathname->dirname pathname->vicinity)
          (define list-directory-files list-directory)))

      (sagittarius
        (import (sagittarius)
                (util file)
                (only (srfi 1) filter))
        (begin
          ; current-directory exported
          (define (make-directory str) (create-directory str))
          (define (pathname->dirname path)
            (let-values (((dir file ext) (decompose-path path)))
                        (if (string? dir)
                          dir
                          "")))
          (define (list-directory-files dir)
            (filter file-regular? (read-directory dir)))))


      (mit
       (begin
        (define current-directory pwd)
        (define pathname->dirname pathname-directory)
        (define list-directory-files directory-read)
        ))

      (stklos
       (begin
         (define pathname->dirname dirname)
         ))

      (chibi)

      (else
        (error "(slib directory) not supported for current R7RS Scheme implementation")))

    (cond-expand
      (guile
       ;; Where possible, iterating over directories with
       ;; `DIRECTORY-FOR-EACH` is done lazily so as not to load all
       ;; directory entries into memory before iterating. On most
       ;; platforms, this also indicates to the operating system that the
       ;; directory is being used by this process while it iterates over
       ;; entries.

       (begin
         (define directory-for-each
           (case-lambda
             ((proc dirname) (directory-for-each proc dirname identity))
             ((proc dirname pred)
              (let ((stream (opendir dirname)))
                (let loop ()
                  (let ((next (readdir stream)))
                    (cond
                     ((eof-object? next) (closedir stream) (values))
                     ((pred next) (proc next) (loop))
                     (else (loop))
                     ))))
              ))))
       )

      (else
       ;; The default implementation of `DIRECTORY-FOR-EACH` iterates over
       ;; a list of directory entry strings that were all collected into
       ;; memory by the `LIST-DIRECTORY-FILES` procedure (which is not
       ;; exported).

       (begin
         (define directory-for-each
           (case-lambda
             ((proc dir) (directory-for-each proc dir identity))
             ((proc dir given-selector)
              (let ((selector (cond ((null? given-selector) identity)
                                    ((procedure? given-selector) given-selector)
                                    ((string? given-selector) (filename:match?? given-selector))
                                    (else (error "Invalid selector for directory-for-each")))))
                (for-each (lambda (filename) (when (selector filename) (proc filename)))
                          (list-directory-files dir)))))))
       ))


    (begin
      (define (directory*-for-each proc path-glob)
        (let* ((dir (pathname->dirname path-glob))
               (glob (string-copy path-glob (string-length dir))))
          (directory-for-each proc
                              (if (equal? "" dir) "." dir)
                              glob))))

    ;;----------------------------------------------------------------
    ))
