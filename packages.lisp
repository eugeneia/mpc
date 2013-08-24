;;;; Package definitions for MPC.

(defpackage mpc
  (:documentation
"Monadic parser combinators. This package contains many different parser
combinators ranging from axioms to high level tools. It also contains the
RUN entry function and various macros that help integrate parser
combinators into the common lisp ecosystem. The functions and macros
marked with the {=}-prefix return parsers. Their documentation is written
as if from the resulting parsers perspective. To get an overview, read
[manual.html], then come back for the details.")
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
  (:export :=character
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
