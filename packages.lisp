;;;; Package definitions for MPC.

(defpackage mpc
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

(defpackage mpc.characters
  (:documentation "Parsers for character inputs.")
  (:use :cl
	:mpc)
  (:export :=character
	   :=string
	   :=string-of
	   :=whitespace
	   :=skip-whitespace
	   :=newline
	   :=line))

(defpackage mpc.numerals
  (:documentation "Parsers for character numerals.")
  (:use :cl
	:mpc
	:mpc.characters)
  (:export :=digit
	   :=natural-number
	   :=integer-number))
