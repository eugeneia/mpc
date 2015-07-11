;;;; Parsers for character inputs.

(in-package :mpc.characters)

(defparameter *whitespace* '(#\Tab #\Newline #\Vt #\Ff #\Return #\Space)
  "*Value Type:*

   a _list_ of _characters_.

   *Description:*

   The _value_ of {*whitespace*} is a _list_ of _characters_ considered
   to be _whitespace characters_.")

(defun =character (character &optional (case-sensitive-p t))
  "*Arguments and Values:*

   _character_—a _character_.

   _case-sensitive-p_—a _generalized boolean_. The default is _true_.

   *Description:*

   {=character} consumes the next item and succeeds with that item as its
   result if the item is equal to _character_. {=character} is case
   sensitive unless _case-sensitive-p_ is _false_."
  (if case-sensitive-p
      (=satisfies (lambda (x) (char=      x character)))
      (=satisfies (lambda (x) (char-equal x character)))))

(defun =string (string &optional (case-sensitive-p t))
  "*Arguments and Values:*

   _string_—a _string_.

   _case-sensitive-p_—a _generalized boolean_. The default is _true_.

   *Description:*

   {=string} consumes a non-empty sequence of _characters_ and succeeds
   with the _character sequence_ coerced to a _string_ if the result is
   equal to _sting_. {=string} is case sensitive unless
   _case-sensitive-p_ is _false_."
  (if (= 0 (length string))
      (=result "")
      (=let* ((_ (=character (aref string 0) case-sensitive-p))
	      (_ (=string (subseq string 1 (length string))
			  case-sensitive-p)))
	(=result string))))

(defun =string-of (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {=string-of} repeatedly applies _parser_ to the input and succeeds
   with the resulting _character sequence_ coerced to a _string_.
   {=string-of} fails unless _parser_ succeeds at least once."
  (=let* ((characters (=one-or-more parser)))
    (=result (coerce characters 'string))))

(defun =whitespace ()
  "*Description:*

   {=whitespace} consumes the next item and succeeds with that item as
   its result if the item is a member of {*whitespace*}."
  (=one-of *whitespace*))

(defun =skip-whitespace (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {=skip-whitespace} consumes a sequence of zero or more items which are
   members of {*whitespace*} and then applies _parser_ and, if
   successful, succeeds with its result."
  (=and (=zero-or-more (=whitespace)) parser))

(defun =newline ()
  "*Description:*

   {=newline} consumes the next item and succeeds with that item as its
   result if the item is the {#\\\\Newline} _character_."
  (=character #\Newline))

(defun =line (&optional keep-newline-p)
  "*Arguments and Values:*

   _keep-newline-p_—a _generalized boolean_. The default is _false_.

   *Description:*

   {=line} consumes a sequence of zero or more _characters_ terminated by
   a {#\\\\Newline} _character_ and succeeds with the _characters_
   coerced to a _string_. The terminating {#\\\\Newline} _character_ is
   not included in the result unless _keep-newline-p_ is _true_."
  (=let* ((line (=or (=string-of (=not (=or (=newline)
					    (=end-of-input))))
		     (=result "")))
	  (newline (=maybe (=newline))))
    (=result (if (and keep-newline-p newline)
		 (format nil "~a~%" line)
		 line))))
