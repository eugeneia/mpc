;;;; Smug parsers for character inputs.

(in-package :smug.characters)

(defparameter *whitespace* '(#\Tab #\Newline #\Vt #\Ff #\Return #\Space)
  "Whitespace characters.")

(defun =character (x &optional (case-sensitive-p t))
  "Returns a parser which parses character {X} with respect to
CASE-SENSITIVE-P, otherwise it fails."
  (if case-sensitive-p
      (=satisfies (lambda (y) (char= x y)))
      (=satisfies (lambda (y) (char-equal x y)))))

(defun =string (string &optional (case-sensitive-p t))
  "Returns a parser which parses STRING with respect to
CASE-SENSITIVE-P, otherwise it fails."
  (if (= 0 (length string))
      (=result "")
      (=let* ((_ (=character (aref string 0) case-sensitive-p))
	      (_ (=string (subseq string 1 (length string))
			  case-sensitive-p)))
	(=result string))))

(defun =string-of (parser)
  "Returns a parser which successfully =BINDs PARSER one or more
times and which returns its results in a string, otherwise it fails."
  (=bind (=one-or-more parser)
	(lambda (s) (=result (coerce s 'string)))))

(defun =whitespace ()
  "Returns a parser which parses {#\\\\Tab}, {#\\\\Newline}, {#\\\\Vt},
{#\\\\Ff}, {#\\\\Return} or {#\\\\Space}."
  (=one-of *whitespace*))

(defun =skip-whitespace (parser)
  "Returns a parser which =BINDs {(=WHITESPACE)} zero or more times and
PARSER, otherwise it fails."
  (=and (=zero-or-more (=whitespace)) parser))

(defun =newline ()
  "Returns a parser which parses {#\\\\Newline}, otherwise it =FAILs."
  (=character #\Newline))

(defun =line (&optional keep-newline)
  "Returns a parser which parses zero or more characters terminated by a
newline character or end of input and which returns them in a string,
otherwise it fails. If KEEP-NEWLINE is not NIL the eventual newline
character is kept in the string."
  (=let* ((line (=or (=string-of (=not (=or (=newline)
					    (=end-of-input))))
		     (=result "")))
	  (newline (=maybe (=newline))))
    (=result (if (and keep-newline newline)
		 (format nil "~a~%" line)
		 line))))
