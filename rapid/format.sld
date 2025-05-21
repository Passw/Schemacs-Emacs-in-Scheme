;; Copyright (C) 2017 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;> Basic format strings compatible with SRFI 28.

(define-library (rapid format)
  (export format)
  (import (scheme base)
	        (scheme write)
	        (rapid assume))

  (begin

    (define (format format-string . objects)
      (let ((buffer (open-output-string)))
        (let loop ((format-list (string->list format-string))
                   (objects objects))
          (cond
           ((null? format-list) (get-output-string buffer))
           ((char=? (car format-list) #\~)
            (assume (not (null? (cdr format-list)))
              "format-string: an escape sequence consists of two characters")
            (case (cadr format-list)
              ((#\a)
               (assume (not (null? objects))
                 "format-string: for each instance of ~a or ~s one value has to be present")
               (display (car objects) buffer)
               (loop (cddr format-list) (cdr objects)))
              ((#\s)
               (assume (not (null? objects))
                 "format-string: for each instance of ~a or ~s one value has to be present")
               (write (car objects) buffer)
               (loop (cddr format-list) (cdr objects)))
              ((#\%)
               (newline buffer)
               (loop (cddr format-list) objects))
              ((#\~)
               (write-char #\~ buffer)
               (loop (cddr format-list) objects))
              (else
               (assume #f "format-string: valid escape sequences are ~a, ~s, ~%, and ~~"))))
           (else
            (write-char (car format-list) buffer)
            (loop (cdr format-list) objects))))))

    ))
