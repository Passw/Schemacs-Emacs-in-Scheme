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

;;> A pattern matcher.

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

;;> This pattern matcher is a portable version of
;;> \hyperlink[http://www.cs.indiana.edu/chezscheme/match/]{Chez Scheme's match}
;;> by Dan Friedman, Erik Hilsdale, and Kent Dybvig.

;;> \section{Pattern matcher}

;;> A \scheme{match} expression looks a lot like a
;;> \scheme{case} expression, except that the keys
;;> are replaced with a pattern to match against the
;;> input.

;;> A match expression has the general form\br{}
;;> \scheme{(match <expr> <clause> ...)},
;;> where \scheme{<expr>} in the input expression
;;> to match and each clause has one of the forms\br{}
;;> \scheme{(<pattern> <body1> <body2> ...)}, and\br{}
;;> \scheme{(<pattern> (guard <guard-expr>) <body1> <body2> ...)}.

;;> As with \scheme{case}, the input expression is evaluated to produce
;;> the input value and the first clause the input value matches, if any,
;;> is selected.
;;> The body \scheme{<body1> <body2> ...} of the selected clause is
;;> evaluated in sequence, and the values of the last expression are
;;> returned.

;;> An input value matches a clause if it fits the clause's pattern and
;;> the guard expression \scheme{<guard-expr>}, if any, evaluates to a
;;> true value.

;;> Patterns may contain symbolic constants, which must match exactly,
;;> and pattern variables, which match any input.  Pattern variables
;;> are prefixed by commas; symbolic constants are not.
;;> \example{
;;> (match '(a 17 37)
;;>   ((a ,x) 1)
;;>   ((b ,x ,y) 2)
;;>   ((a ,x ,y) 3))
;;> }

;;> The first clause fails to match because there are three items in the
;;> input list, and the pattern has only two.  The second clause fails
;;> because \scheme{b} does not match \scheme{a}.

;;> In the output expression, the values of the pattern variables are
;;> bound to the corresponding pieces of input.
;;> \example{
;;> (match '(a 17 37)
;;>   ((a ,x) (- x))
;;>   ((b ,x ,y) (+ x y))
;;>   ((a ,x ,y) (* x y)))
;;> }

;;> When followed by an ellipsis (\scheme{...}), a pattern variable
;;> represents a sequence of input values.
;;> \example{(match '(a 17 37) ((a ,x* ...) x*))}

;;> Ellipses can follow a structures pattern containing one or more
;;> pattern variables.
;;> \example{
;;> (match '(begin (1 5) (2 6) (3 7) (4 8))
;;>   ((begin (,x* ,y*) ...) (append x* y*))) 
;;> }

;;> Ellipses can be nested, producing sequences of sequences of values.
;;> \example{
;;> (match '((a b c d) (e f g) (h i) (j))
;;>   (((,x* ,y** ...) ...) (list x* y**)))
;;> }

;;> Recursion is frequently required while processing an input expression
;;> with \scheme{match}.  Here, a procedure returning the length of a list
;;> is defined.
;;> \example{
;;> (letrec
;;>     ((len
;;>       (lambda (lst)
;;>         (match lst
;;>           (() 0)
;;>           ((,x ,x* ...) (+ 1 (len x*)))))))
;;>   (len '(a b c d)))
;;> }

;;> A simpler version of the above uses the catamorphism feature of
;;> \scheme{match}. If a pattern variable is written as \scheme{,(<var>)},
;;> \scheme{match} recurs on the matching subpart of the input before
;;> evaluating the output expressions of the clause.
;;> \example{
;;> (let ((len
;;>        (lambda (lst)
;;>          (match lst
;;>            (() 0)
;;>            ((,x . ,(x*)) (+ 1 x*))))))
;;>   (len '(a b c d)))
;;> }

;;> In some cases, \scheme{match} will need to return multiple values.
;;> The catamorphism syntax can be used to receive multiple values.
;;> When making implicit recursive calls using the catamorphism syntax,
;;> zero or more variables between the brackets can be included, each
;;> representing one of the expected return values.
;;>
;;> \example{
;;> (let ((split
;;>        (lambda (lis)
;;>          (match lis
;;>            (() (values '() '()))
;;>            ((,x) (values `(,x) '()))
;;>            ((,x ,y . ,(odds evens)) (values `(,x . ,odds)
;;>                                             `(,y . ,evens)))))))
;;>   (split '(a b c d e f)))
;;> }

;;> Sometimes it is useful to explicitely name the operator in a catamorphism
;;> subpattern.  Whereas \scheme{,(<var> ...)} recurs to the top of the
;;> current \scheme{match}, \scheme{,(<cata> -> <var> ...)} recurs to
;;> \scheme{<cata>}.  \scheme{<Cata>} must evaluate to a procedure that
;;> accepts one argument, the input expression, and returns as many
;;> values as there are identifiers following the \scheme{->}.

;;> Here is an example that illustrates the use of guards and how to achieve the
;;> effect of a catch-all clause.
;;> \example{
;;> (let ((simple-eval
;;>        (lambda (x)
;;>          (match x
;;>            (,i (guard (integer? i)) i)
;;>            ((+ ,(x*) ...) (apply + x*))
;;>            ((* ,(x*) ...) (apply * x*))
;;>            ((- ,(x) ,(y)) (- x y))
;;>            ((/ ,(x) ,(y)) (/ x y))
;;>            (,x (error "simple-eval: invalid expression" x))))))
;;>   (simple-eval '(+ (- 0 1) (+ 2 3))))
;;> }

;;> \section{Syntax}

;;> \macro{(match <expr> <clause> ...)}
;;> The result of \scheme{<expr>} is matched against each
;;> \scheme{<clause>} in turn,
;;> according to the rules described in the previous section,
;;> until the the first clause matches.  When a match is found, the
;;> corresponding body is evaluated, and the values of the last expression are
;;> returned as the result of the entire match.  It is an error if no pattern
;;> matches.

(define-library (rapid match)
  (import
    (rename (scheme base) (|...| ellipsis))
    (except (scheme base) |...|)
    (rapid assume))

  (cond-expand
    (guile
     (export
      match match* |->| unquote guard
      ))
    (else
     (export
      match match* |->| unquote guard
      (rename ellipsis |...|)
      ))
    )

  (begin

    (define-syntax match
      (syntax-rules ... ()
        ((match . args)
         (match* (#f #f) . args))))

    ;;> \macro{(match* (<unbox> <loop>) <expr> <clause> ...)}

    ;;> Equivalent to the \scheme{(match <expr> <clause> ...)} with the
    ;;> following exceptions:  If \scheme{<unbox>} is not \scheme{#f},
    ;;> each input expression is converted with \scheme{<unbox>}
    ;;> before matched against a list or vector pattern.  If \scheme{<loop>}
    ;;> is not \scheme{#f}, catamorphism subpatterns recur to \scheme{<loop>}
    ;;> if no explicit operator is named.

    (define-syntax match*
      (syntax-rules ... ()
        ((match* (unbox loop) expr . clauses)
         (match-aux "unbox" unbox loop expr clauses))))

    ;; Helper syntax

    (define-syntax match-aux
      (syntax-rules ... ()
        ((match-aux "unbox" #f loop . args)
         (match-aux "loop" identity loop . args))
        ((match-aux "unbox" unbox loop . args)
         (let ((%unbox unbox))
           (match-aux "loop" %unbox loop . args)))
        ((match-aux "loop" unbox #f expr . args)
         (let loop ((%expr expr))
           (match-expr unbox loop %expr . args)))
        ((match-aux "loop" unbox loop expr . args)
         (let ((%loop loop)
         (%expr expr))
           (match-expr unbox %loop %expr . args)))))

    (define-syntax match-expr
      (syntax-rules ... ()
        ((match-expr unbox loop expr clauses)
         (match-expr unbox loop expr return fail clauses ()))
        ((match-expr unbox loop expr return fail ()
              (((var ...) matcher condition (((cata-var ...) cata tmp) ...) body) ...))
         (call/cc
          (lambda (return)
      (call/cc
       (lambda (fail)
         (let-values (((var ...) (matcher expr)))
           (fend fail
                 condition
           (let-values (((cata-var ...) (cata tmp)) ...)
             (call-with-values
           (lambda () . body)
               return))
           ))))
      ...
      (assume #f "input expression must match one clause" expr))))
        ((match-expr unbox loop expr return fail (clause . clauses) compiled-clauses)
         (compile-clause (match-expr unbox loop expr return fail clauses compiled-clauses)
             unbox loop expr fail clause))

        ((match-expr unbox loop expr return fail clauses (compiled-clause ...)
              condition body vars binding matcher)
         (match-expr unbox loop expr return fail clauses
              (compiled-clause ... (vars matcher condition binding body))))))

    (define-syntax compile-clause
      (syntax-rules ... (guard)
        ((compile-clause k* unbox loop expr fail (pattern (guard condition) . body))
         (compile-clause k* unbox loop expr fail pattern condition body))
        ((compile-clause k* unbox loop expr fail (pattern . body))
         (compile-clause k* unbox loop expr fail pattern #t body))
        ((compile-clause (k ...) unbox loop expr fail pattern condition body)
         (compile-pattern (k ... condition body)
              unbox loop fail pattern))))

    (define-syntax compile-pattern
      (syntax-rules ... (-> unquote ellipsis)
        ((compile-pattern (k ...) unbox loop fail ,(cata -> var ...))
         (k ... (tmp) (((var ...) cata tmp)) (lambda (expr)
                  expr)))
        ((compile-pattern (k ...) unbox loop fail ,(var ...))
         (k ... (tmp) (((var ...) loop tmp)) (lambda (expr)
                   expr)))
        ((compile-pattern (k ...) unbox loop fail ,var)
         (k ... (var) () (lambda (expr) expr)))
        ((compile-pattern (k ...) unbox loop fail ())
         (k ... () () (lambda (expr)
            (unless (null? (unbox expr))
              (fail))
            (values))))
        ((compile-pattern k* unbox loop fail #(pattern ...))
         (split-vector (compile-pattern k* unbox loop fail) (pattern ...)))
        ((compile-pattern k* unbox loop fail elements () ())
         (compile-patterns (compile-vector-pattern k* unbox loop fail)
               unbox loop fail elements))
        ((compile-pattern k* unbox loop fail (element1 ...) (element2) elements)
         (compile-patterns (compile-vector-pattern-ellipsis k* unbox loop fail elements)
               unbox loop fail (element1 ... element2)))
        ((compile-pattern k* unbox loop fail (pattern1 ellipsis pattern2 ... . ,pattern*))
         (compile-patterns (compile-list-pattern-ellipsis k* unbox loop fail (length '(pattern2 ...)))
               unbox loop fail (pattern1 (pattern2 ... . ,pattern*))))
        ((compile-pattern k* unbox loop fail (pattern1 ellipsis pattern2 ... . pattern*))
         (compile-patterns (compile-list-pattern-ellipsis k* unbox loop fail (length '(pattern2 ...)))
               unbox loop fail (pattern1 (pattern2 ... . pattern*))))
        ((compile-pattern k* unbox loop fail (pattern . pattern*))
         (compile-patterns (compile-list-pattern k* unbox loop fail)
               unbox loop fail (pattern pattern*)))
        ((compile-pattern (k ...) unbox loop fail literal)
         (k ... () () (lambda (expr)
            (unless (eq? 'literal (unbox expr))
              (fail))
            (values))))))

    (define-syntax compile-patterns
      (syntax-rules ... ()
        ((compile-patterns k* unbox loop fail patterns)
         (compile-patterns k* unbox loop fail patterns 0 () ()))
        ((compile-patterns (k ...) unbox loop fail () n compiled-patterns vars)
         (k ... n compiled-patterns vars))
        ((compile-patterns k* unbox loop fail (pattern . patterns) n compiled-patterns vars)
         (compile-pattern
          (compile-patterns k* unbox loop fail patterns n compiled-patterns vars)
          unbox loop fail pattern))
        ((compile-patterns k* unbox loop fail patterns n (compiled-pattern ...) (var ...)
               (var0 ...) (binding0 ...) matcher)
         (compile-patterns k* unbox loop fail patterns (+ 1 n)
               (compiled-pattern ... ((var0 ...) (binding0 ...) matcher n))
               (var ... var0 ...)))))

    (define-syntax compile-vector-pattern
      (syntax-rules ... ()
        ((compile-vector-pattern k* unbox loop fail n
               (((var ...) (((cata-var ...) cata tmp) ...) matcher i) ...)
               vars)
         (flatten (compile-vector-pattern k* unbox loop fail n
                  (((var ...) (((cata-var ...) cata tmp) ...) matcher i) ...)
                  vars)
            ((((cata-var ...) cata tmp) ...) ...)))
        ((compile-vector-pattern (k ...) unbox loop fail n
               (((var ...) (((cata-var ...) cata tmp) ...) matcher i) ...)
               vars cata-vars)
         (k ... vars cata-vars
      (lambda (expr)
        (unless (vector? expr) (fail))
        (unless (= (vector-length expr) n) (fail))
        (let-values (((var ...) (matcher (vector-ref expr i))) ...)
          (values . vars)))))))

    (define-syntax compile-vector-pattern-ellipsis
      (syntax-rules ...  ()
        ((compile-vector-pattern-ellipsis k* unbox loop fail
                  elements
                  n
                  compiled-elements
                  vars)
         (compile-patterns (compile-vector-pattern-ellipsis k* unbox loop fail
                  n
                  compiled-elements
                  vars)
               unbox loop fail
               elements))
        ((compile-vector-pattern-ellipsis
          k* unbox loop fail n1
          (((var1 ...) (((cata-var1 ...) cata1 tmp1) ...) matcher1 i1) ...
           ((var ...) (((cata-var ...) cata tmp) ...) matcher _))
          (vars1 ...)
          n2
          (((var2 ...) (((cata-var2 ...) cata2 tmp2) ...) matcher2 i2) ...)
          (vars2 ...))
         (flatten
          (compile-vector-pattern-ellipsis
           k* unbox loop fail n1
           (((var1 ...) (((cata-var1 ...) cata1 tmp1) ...) matcher1 i1) ...
      ((var ...) (((cata-var ...) cata tmp) ...) matcher _))
           (vars1 ...)
           n2
           (((var2 ...) (((cata-var2 ...) cata2 tmp2) ...) matcher2 i2) ...)
           (vars2 ...))
          ((((cata-var1 ...) cata1 tmp1) ...) ...
           (((cata-var2 ...) cata2 tmp2) ...) ...)))
        ((compile-vector-pattern-ellipsis
          (k ...) unbox loop fail n1
          (((var1 ...) (((cata-var1 ...) cata1 tmp1) ...) matcher1 i1) ...
              ((var ...) (((cata-var ...) cata tmp) ...) matcher _))
                   (vars1 ...)
                   n2
                   (((var2 ...) (((cata-var2 ...) cata2 tmp2) ...)
               matcher2 i2) ...)
                   (vars2 ...)
                   cata-var*)
         (k ... (vars1 ... vars2 ...)
      (((cata-var ...)
        (lambda (tmp)
          (map-values cata tmp (length '(cata-var ...))))
        tmp)
       ...
       . cata-var*)
      (lambda (expr)
        (let ((expr (unbox expr)))
          (unless (vector? expr) (fail))
          (let ((l (- (vector-length expr) (+ n1 n2 -1))))
            (when (negative? l) (fail))
            (let-values (((var1 ...) (matcher1 (vector-ref expr i1)))
             ...
             ((var ...) (map-values matcher
                  (vector->list expr (+ n1 -1) (+ n1 l -1))
                  (length '(var ...))))
             ((var2 ...) (matcher2 (vector-ref expr (+ i2 n1 l -1))))
             ...)
        (values vars1 ... vars2 ...)))))))))

    (define-syntax compile-list-pattern
      (syntax-rules ... ()
        ((compile-list-pattern (k ...) unbox loop fail _
             (((var ...) (((cata-var ...) cata tmp) ...) matcher _)
              ((var* ...) (((cata-var* ...) cata* tmp*) ...) matcher* _))
             vars)
         (k ... vars (((cata-var ...) cata tmp) ...
          ((cata-var* ...) cata* tmp*) ...)
      (lambda (expr)
        (let ((pair (unbox expr)))
          (unless (pair? pair) (fail))
          (let-values (((var ...) (matcher (car pair)))
           ((var* ...) (matcher* (cdr pair))))
            (values var ... var* ...))))))))

    (define-syntax compile-list-pattern-ellipsis
      (syntax-rules ... ()
        ((compile-list-pattern-ellipsis (k ...) unbox loop fail len _
                (((var ...) (((cata-var ...) cata tmp) ...) matcher _)
                 ((var* ...) (((cata-var* ...) cata* tmp*) ...) matcher* _))
                vars)
         (k ... vars (((cata-var ...)
           (lambda (tmp)
             (map-values cata tmp (length '(cata-var ...))))
           tmp) ...
          ((cata-var* ...) cata* tmp*) ...)
      (lambda (expr)
        (let-values (((head tail) (split expr len unbox fail)))
          (let-values ((v* (matcher* tail)))
            (let-values (((var ...) (map-values matcher
                  head (length '(var ...)))))
        (apply values var ... v*)))))))))

    (define-syntax split-vector
      (syntax-rules ... (ellipsis)
        ((split-vector k* elements)
         (split-vector k* elements ()))
        ((split-vector (k ...) (element2 ellipsis . element*) element1*)
         (k ... element1* (element2) element*))
        ((split-vector k* (element2 . element*) (element1 ...))
         (split-vector k* element* (element1 ... element2)))
        ((split-vector (k ...) () element1*)
         (k ... element1* () ()))))

    (define-syntax flatten
      (syntax-rules ... ()
        ((flatten k* x* )
         (flatten k* x* ()))
        ((flatten (k ...) () y**)
         (k ... y**))
        ((flatten k* (x* ... (a ...)) y**)
         (flatten k* (x* ...) (a ... . y**)))))

    (define-syntax fend
      (syntax-rules ... ()
        ((fend fail #t expr)
         expr)
        ((fend fail condition expr)
         (if condition expr (fail)))))

    ;;> \section{Auxiliary syntax}

    ;;> \macro{unquote\br{}
    ;;> ,}
    ;;> \macro{...}
    ;;> \macro{guard}
    ;;> \macro{->}
    ;;> Auxiliary syntax.

    (define-syntax ->
      (syntax-rules ... ()
        ((-> . _) (syntax-error "invalid use of auxiliary syntax" ->))))   

    ;;; Utility procedures

    (define (identity value) value)

    (define (split list i unbox fail)
      (let loop ((i i) (end list))
        (if (zero? i)
      (let loop ((list list) (end end) (hare (unbox end)))
        (let* ((end (unbox end))
         (front (unbox list))
         (hare (if (pair? hare) (unbox (cdr hare))))
         (hare (if (pair? hare) (unbox (cdr hare)))))
          (if (pair? end)
        (if (eq? hare end)
            (fail)		      
            (let-values (((head tail)
              (loop (cdr front) (cdr end) hare)))
              (values (cons (car front) head) tail)))
        (values '() (if (null? front)
            '()
            list)))))
      (let ((end (unbox end)))
        (if (pair? end)
            (loop (- i 1) (cdr end))
            (fail))))))

    (define (map-values proc lst . len*)
      (if (null? lst)
          (apply values (make-list (car len*) '()))  
          (apply values
           (apply map list 
            (map (lambda (obj)
             (let-values ((x* (proc obj)))
               x*))
           lst)))))

    ;;----------------------------------------------------------------
    ))


;; Local Variables:
;; eval: (put 'match 'scheme-indent-function 1)
;; eval: (put 'match* 'scheme-indent-function 2)
;; eval: (font-lock-add-keywords 'scheme-mode
;;                               '(("(\\(match\\)\\>" 1 font-lock-keyword-face)
;;                                 ("(\\(match\\*\\)\\>" 1 font-lock-keyword-face)))
;; End:
