;;;; Parsers for character inputs.

(in-package :mpc.characters)

(defparameter *whitespace* '(#\Tab #\Newline #\Vt #\Ff #\Return #\Space)
  "Whitespace characters.")

(defun =character (character &optional (case-sensitive-p t))
  "Consume and return CHARACTER or fail. If CASE-SENSIVE-P is _true_ then
CHAR= is used for comparison, otherwise its case insensitive counterpart
CHAR-EQUAL is used. CASE-SENSISTIVE-P is T by default."
  (if case-sensitive-p
      (=satisfies (lambda (x) (char=      x character)))
      (=satisfies (lambda (x) (char-equal x character)))))

(defun =string (string &optional (case-sensitive-p t))
  "Consume and return STRING of characters. If CASE-SENSIVE-P is _true_
then CHAR= is used for comparison, otherwise its case insensitive
counterpart CHAR-EQUAL is used. CASE-SENSISTIVE-P is T by default."
  (if (= 0 (length string))
      (=result "")
      (=let* ((_ (=character (aref string 0) case-sensitive-p))
	      (_ (=string (subseq string 1 (length string))
			  case-sensitive-p)))
	(=result string))))

(defun =string-of (parser)
  "Apply PARSER as if by =ONE-OR-MORE and return the results coerced to a
string."
  (=let* ((characters (=one-or-more parser)))
    (=result (coerce characters 'string))))

(defun =whitespace ()
  "Consume and return one of {#\\\\Tab}, {#\\\\Newline}, {#\\\\Vt},
{#\\\\Ff}, {#\\\\Return} or {#\\\\Space}, otherwise fail."
  (=one-of *whitespace*))

(defun =skip-whitespace (parser)
  "Apply {(=WHITESPACE)} zero or more times, then apply PARSER,
effectively discarding leading whitespace characters."
  (=and (=zero-or-more (=whitespace)) parser))

(defun =newline ()
  "Parses {#\\\\Newline} or fail."
  (=character #\Newline))

(defun =line (&optional keep-newline)
  "Consume and return zero or more characters terminated by a
newline character or end of input as a string. When KEEP-NEWLINE is
_true_ and the line is terminated by a newline character, include it in
the returned string. KEEP-NEWLINE is NIL by default."
  (=let* ((line (=or (=string-of (=not (=or (=newline)
					    (=end-of-input))))
		     (=result "")))
	  (newline (=maybe (=newline))))
    (=result (if (and keep-newline newline)
		 (format nil "~a~%" line)
		 line))))
