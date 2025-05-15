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

  (include "directory.scm")
  )
