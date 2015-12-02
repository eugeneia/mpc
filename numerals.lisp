;;;; Parsers for character numerals.

(in-package :mpc.numerals)

(defun =digit (&optional (radix 10))
  "*Arguments and Values:*

   _radix_—a _number_ of _type_ {(integer 2 36)}. The default is {10}.

   *Description:*

   {=digit} consumes the next item and succeeds with that item as its
   result if the next item is a digit _character_ in the specified
   _radix_."
  (=satisfies (lambda (x) (digit-char-p x radix))))

(defun =natural-number (&optional (radix 10))
  "*Arguments and Values:*

   _radix_—a _number_ of _type_ {(integer 2 36)}. The default is {10}.

   *Description:*

   {=natural-number} consumes a non-empty sequence of digit _characters_
   in the specified _radix_ and succeeds with the natural _number_
   represented by that sequence."
  (=let* ((numeric (=string-of (=digit radix))))
    (=result (parse-integer numeric :radix radix))))

(defun =integer-number (&optional (radix 10))
  "*Arguments and Values:*

   _radix_—a _number_ of _type_ {(integer 2 36)}. The default is {10}.

   *Description:*

   {=integer-number} consumes a signed non-empty sequence of digit
   _characters_ in the specified _radix_ and succeeds with the _integer_
   represented by that sequence. The leading sign is optional and can be
   {#\\\\+} and {#\\\\-} for positive and negative values
   respectively. The default is a positive value."
  (=let* ((sign (=maybe (=one-of '(#\+ #\-))))
          (n (=natural-number radix)))
    (=result (case sign
               (#\- (- n))
               ((#\+ nil) n)))))
