;;;; Package definitions for MPC.

(defpackage mpc
  (:documentation
"Monadic parser combinators. This package contains general purpose parser
combinators of varying sophistication. It also contains the {run} entry
function and a handful of macros that integrate parser combinators into the
Common Lisp condition system. Functions starting with the {=}-prefix construct
_parsers_. Their documentation is written from the perspective of the resulting
parser. Refer to the [MPC Manual](manual.html) for a general introduction.")
  (:use :cl)
  (:export :=end-of-input
	   :=result
	   :=fail
	   :=item
	   :=bind
	   :=plus
	   :=let*
	   :=or
	   :=and
	   :=if
	   :=when
	   :=unless
	   :=not
	   :=prog1
	   :=prog2
	   :=list
	   :=maybe
	   :=satisfies
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
  (:documentation
   "This package includes parsers specialised for character input.
Covered are case sensitivity, strings, whitespace and lines.")
  (:use :cl
	:mpc)
  (:export :*whitespace*
           :=character
	   :=string
	   :=string-of
	   :=whitespace
	   :=skip-whitespace
	   :=newline
	   :=line))

(defpackage mpc.numerals
  (:documentation "This package includes parsers for string numerals.
Covered are single digits, natural numbers and signed integers with
arbitrary radixes.")
  (:use :cl
	:mpc
	:mpc.characters)
  (:export :=digit
	   :=natural-number
	   :=integer-number))
