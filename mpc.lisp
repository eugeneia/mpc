;;;; Monadic parser primitives and utility parsers.

(in-package :mpc)

(defvar *input-at-run* nil
  "Bound to intial input during RUN.")

(defvar *input-during-fail* nil
  "Bound to input during =FAIL.")

(defun =end-of-input ()
  "Succeeds when input is empty and fail otherwise."
  (declare (optimize (speed 3)))
  (lambda (input)
    (when (input-empty-p input)
      (list (cons t input)))))

(defun =result (value)
  "Always succeed without consuming input and return VALUE ."
  (declare (optimize (speed 3)))
  (lambda (input)
    (list (cons value input))))

(defmacro =fail (&rest handling)
  "Fail and optionally execute HANDLING. GET-INPUT-POSITION may be called
inside HANDLING."
  (if handling
      (let ((input (gensym "INPUT")))
	`(lambda (,input)
	   (let ((*input-during-fail* ,input))
	     ,@handling)
	   nil))
      '(constantly nil)))

(defun =item ()
  "Consume and return the next item from input or fail if input is empty."
  (lambda (input)
    (unless (input-empty-p input)
      (list (cons (input-first input)
		  (input-rest input))))))

(defun =bind (parser make-parser)
  "Apply PARSER. For each resulting {(VALUE . INPUT)} pair apply
MAKE-PARSER to each VALUE and apply the resulting parser to each
INPUT. Return the concatenated results."
  (declare (optimize (speed 3)))
  (lambda (input)
    (loop for (value . input) in (funcall parser input)
       append (funcall (funcall make-parser value) input))))

(defun =plus (&rest parsers)
  "The non-deterministic choice combinator. Apply PARSERS to input and
returns a list of their results."
  (lambda (input)
    (loop for parser in parsers 
       append (funcall parser input))))

;;; =LET* is the natural syntax for lispers
(defmacro =let* (bindings &body body)
  "A mix of =AND and LET*. Apply the parsers in BINDINGS as if by
=AND and bind their results to the symbols in BINDINGS. If the symbol is
{_} then ignore the result. Finally apply parsers in BODY with BINDINGS
in an implicit PROGN."
  (if bindings
      (let ((symbol (first (first bindings))))
	`(=bind ,@(cdr (first bindings))
                (lambda (,symbol)
                  ,@(when (string-equal (symbol-name symbol) "_")
                          `((declare (ignore ,symbol))))
                  (=let* ,(cdr bindings)
		 ,@body))))
      `(progn ,@body)))

(defun =or (&rest parsers)
  "Apply PARSERS to until a parser is successful and return its result or
fail."
  (declare (optimize speed))
  (labels ((non-consing-or (parsers)
	     (lambda (input)
	       (or (funcall (the function (first parsers)) input)
		   (when (rest parsers)
		     (funcall (the function
				(non-consing-or (rest parsers)))
			      input))))))
    (if parsers
        (non-consing-or parsers)
        (constantly nil))))

(defun =binary-and (parser-a parser-b)
  "=BIND PARSER-A and PARSER-B and fail if any of those fails. Otherwise
return the result of PARSER-B."
  (=let* ((_ parser-a))
    parser-b))

(defun =and (&rest parsers)
  "Apply PARSERS sequentially. If all PARSERS succeed return the last
  PARSERS result. Otherwise fail."
  (let ((p (first parsers))
	(ps (rest parsers)))
    (if ps
	(=binary-and p (apply #'=and ps))
	(or p (=result nil)))))

(defun =list (&rest parsers)
  "Apply PARSERS like =AND but return their results as a list."
  (if parsers 
      (=let* ((x (first parsers))
	      (xs (apply '=list (rest parsers))))
	(=result (cons x xs)))
      (=result nil)))

(defun =if (test-parser then-parser &optional (else-parser (=fail)))
  "Apply THEN-PARSER if applying TEST-PARSER _would_ succeed. Apply
ELSE-PARSER othwerwise."
  (lambda (input)
    (if (funcall test-parser input)
	(funcall then-parser input)
	(funcall else-parser input))))

(defun =when (test-parser &rest parsers)
  "Apply PARSERS as if by =AND when TEST-PARSER _would_ succeed."
  (=if test-parser (apply #'=and parsers)))

(defun =unless (test-parser &rest parsers)
  "Apply PARSERS as if by =AND unless TEST-PARSER _would_ succeed."
  (=if test-parser (=fail) (apply #'=and parsers)))

(defun =not (parser)
  "Appliy {(=ITEM)} to input unless PARSER _would_ succeed, otherwise
fail."
  (=if parser (=fail) (=item)))

(defun =prog1 (parser &rest parsers)
  "Apply PARSER and PARSERS as if by =AND but return the result of
PARSER."
  (=let* ((result parser)
	  (_ (apply #'=and parsers)))
    (=result result)))

(defun =prog2 (parser1 parser2 &rest parsers)
  "Apply PARSER1, PARSER2 and PARSERS as if by =AND but return the result
of PARSER2."
  (=and parser1 (apply #'=prog1 parser2 parsers)))

(defun =maybe (parser)
  "Apply PARSER and return its result. If PARSER fails return NIL
(e.g. never fail)."
  (=or parser (=result nil)))

(defun =satisfies (predicate)
  "Return next {(ITEM)} if it satisfies PREDICATE, othwerise fail."
  (=bind (=item)
	(lambda (x)
	  (if (funcall predicate x)
	      (=result x)
	      (=fail)))))

(defun =eql (x)
  "Return next {(ITEM)} if it is EQL to X, otherwise fail."
  (=satisfies (lambda (y) (eql x y))))

(defun =one-of (list)
  "Return next {(ITEM)} if it is EQL to any item in LIST, otherwise
fail."
  (=let* ((item (=item)))
    (if (find item list :test #'eql)
	(=result item)
	(=fail))))

(defun =none-of (list)
  "Return next {(ITEM)} if it is _not_ EQL to every item in LIST,
otherwise fail."
  (=not (=one-of list)))

(defun =range (from to &key (parser (=item)) (predicate #'char<=))
  "Return next {(ITEM)} if it it matches the range defined by FROM, TO
and PREDICATE, otherwise fail. PARSER defaults to {(=ITEM)} and PREDICATE
defaults to {#'CHAR<=}."
  (=let* ((char parser))
    (if (funcall predicate from char to)
	(=result char) 
	(=fail))))

(defun =one-or-more (parser)
  "Return a list of results for each time PARSER can successfully be
applied in sequence. Fail if PARSER can not be applied successfully at
least once. "
  (=let* ((x parser)
	  (y (=zero-or-more parser)))
    (=result (cons x y))))

(defun =zero-or-more (parser)
  "Return a list of results for each time PARSER can successfully be
applied in sequence."
  (=maybe (=one-or-more parser)))

(defun =one-to (n parser)
  "Return a list of up to N results for each time PARSER can successfully be
applied in sequence. But fail if PARSER can not be applied successfully
at least once."
  (case n
    (0 (=result nil))
    (t (=let* ((x parser)
	       (xs (=or (=one-to (1- n) parser)
			(=result nil))))
	 (=result (cons x xs))))))

(defun =zero-to (n parser)
  "Return a list of up to N results for each time PARSER can successfully be
applied in sequence."
  (=maybe (=one-to n parser)))

(defun =at-least (n parser &key limit)
  "Return a list of at least N and, if LIMIT is not NIL, up to LIMIT
results for each time PARSER can successfully be applied in sequence,
otherwise fail. LIMIT defaults to NIL."
  (case n
    (0 (if limit
	   (if (zerop limit)
	       (=result nil)
	       (=zero-to limit parser))
	   (=zero-or-more parser)))
    (t (=let* ((x parser)
	       (xs (=at-least (1- n) parser :limit (1- limit))))
	 (=result (cons x xs))))))

(defun =exactly (n parser)
  "Return a list of exactly N results of applying PARSER sequentially,
otherwise fail."
  (=at-least n parser :limit n))

(defun =funcall (parser function)
  "Apply PARSER. If it succeds call FUNCTION on its result and return the
resulting value. Otherwise fail."
  (=let* ((result parser))
    (if result
	(=result (funcall function result))
	(=fail))))

(defun run (parser input-source &key (result #'caar))
  "Run a PARSER against INPUT-SOURCE, which can be an ARRAY, a
STREAM or a LIST. Then call RESULT on the resulting list of {(VALUE
. INPUT)} pairs. RESULT defaults to {#'CAAR} (e.g. return the first
VALUE)."
  (let ((*input-at-run* (make-input input-source)))
    (funcall result (funcall parser *input-at-run*))))
