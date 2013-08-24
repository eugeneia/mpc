;;;; Package definitions for smug.

(defpackage smug
  (:documentation "Monadic parser combinators.")
  (:use :cl)
  (:export :=end-of-input
	   :=result
	   :=fail
	   :=item
	   :=bind
	   :=satisfies
	   :=plus
	   :=let*
	   :=or
	   :=and
	   :=list
	   :=if
	   :=when
	   :=unless
	   :=not
	   :=prog1
	   :=prog2
	   :=maybe
	   :=eql
	   :=one-of
	   :=none-of
	   :=range
	   :=zero-or-more
	   :=one-or-more
	   :=one-to
	   :=zero-to
	   :=at-least
	   :=exactly
	   :=funcall
	   :=handler-case
	   :=restart-case
	   :run
	   :get-input-position))

(defpackage smug.characters
  (:documentation "Smug parsers for character inputs.")
  (:use :cl
	:smug)
  (:export :=character
	   :=string
	   :=string-of
	   :=whitespace
	   :=skip-whitespace
	   :=newline
	   :=line))

(defpackage smug.numerals
  (:documentation "Smug parsers for character numerals.")
  (:use :cl
	:smug
	:smug.characters)
  (:export :=digit
	   :=natural-number
	   :=integer-number))
