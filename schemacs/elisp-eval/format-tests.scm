(import
  (scheme base)
  (srfi 64)
  (gypsum elisp-eval format))

(test-begin "gypsum_elisp_eval_format")

(test-eqv 5 (format-count "%%%%%"))
(test-eqv 3 (format-count "a%b%c%d"))
(test-eqv 0 (format-count ""))
(test-eqv 0 (format-count "abcdef"))
(test-eqv 1 (format-count "%abcde"))
(test-eqv 1 (format-count "abcde%"))
(test-eqv 1 (format-count "%"))

(test-equal "hello world"     (format "hello %s" "world"))
(test-equal "hello 12345"     (format "hello %s" 12345))
(test-equal "hello \"world\"" (format "hello %S" "world"))
(test-equal "hello 12345"     (format "hello %S" 12345))

(test-end "gypsum_elisp_eval_format")
