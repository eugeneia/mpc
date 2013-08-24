;;;; Smug parsers for character numerals.

(in-package :smug.numerals)

(defun =digit (&optional (radix 10))
  "Returns a parser which parses a digit with {RADIX}."
  (=satisfies (lambda (x) (digit-char-p x radix))))

(defun =natural-number (&optional (radix 10))
  "Returns a parser which parses a natural number with {RADIX}."
  (=result (parse-integer (=string-of (=digit radix)) :radix radix)))

(defun =integer-number (&optional (radix 10))
  "Returns a parser which parses an integer with {RADIX}."
  (flet ((op () 
	   (=plus (=let* ((_ (=character #\-)))
		    (=result #'-))
		 (=result #'identity))))
    (=let* ((op (op))
	    (n (=natural-number radix)))
      (=result (funcall op n)))))
