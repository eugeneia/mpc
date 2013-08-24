;;;; Parsers for character numerals.

(in-package :mpc.numerals)

(defun =digit (&optional (radix 10))
  "Consume a digit character and return its value relative to
RADIX. RADIX defaults to {10}."
  (=satisfies (lambda (x) (digit-char-p x radix))))

(defun =natural-number (&optional (radix 10))
  "Consume a sequence of digit characters (e.g. a natural number string)
and return its value relative to RADIX. RADIX defaults to {10}."
  (=let* ((numeric (=string-of (=digit radix))))
    (=result (parse-integer numeric :radix radix))))

(defun =integer-number (&optional (radix 10))
  "Consume a signed ({#\\\\+}, {#\\\\-} or none) sequence of digit
characters (e.g. an integer) and return its value relative to RADIX.
RADIX defaults to {10}."
  (=let* ((sign (=maybe (=one-of '(#\+ #\-))))
          (n (=natural-number radix)))
    (=result (case sign
               (#\- (- n))
               ((#\+ nil) n)))))
