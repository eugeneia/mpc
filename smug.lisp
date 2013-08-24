;;;; Monadic parser primitives and utility parsers.

(in-package :smug)

(defvar *input-at-run* nil
  "Bound to intial input during RUN.")

(defvar *input-during-fail* nil
  "Bound to input during =FAIL.")

(defun =end-of-input ()
  "Returns a parser which succeeds when input is empty and fails
otherwise."
  (declare (optimize (speed 3)))
  (lambda (input)
    (when (input-empty-p input)
      (list (cons t input)))))

(defun =result (value)
  "Returns a parser that always returns VALUE without consuming input."
  (declare (optimize (speed 3)))
  (lambda (input)
    (list (cons value input))))

(defmacro =fail (&rest handling)
  "Returns a parser which fails and optionally executes HANDLING.
GET-INPUT-POSITION may be called inside HANDLING."
  (if handling
      (let ((input (gensym "INPUT")))
	`(lambda (,input)
	   (let ((*input-during-fail* ,input))
	     ,@handling)
	   nil))
      '(constantly nil)))

(defun =item ()
  "Returns a parser which consumes and returns the next item from input
or fails if input is empty."
  (lambda (input)
    (unless (input-empty-p input)
      (list (cons (input-first input)
		  (input-rest input))))))

(defun =bind (parser make-parser)
  "Returns a parser that first applies PARSER to the input, returning a
list of {(VALUE . INPUT)} pairs. MAKE-PARSER is then applied to each
VALUE and the resulting parser is then applied to each INPUT. The
returned lists of pairs are then concatenated and returned."
  (declare (optimize (speed 3)))
  (lambda (input)
    (loop for (value . input) in (funcall parser input)
       append (funcall (funcall make-parser value) input))))

(defun =plus (&rest parsers)
  "The non-deterministic choice combinator. Returns a parser that applies
PARSERS to input and returns a list of their results."
  (lambda (input)
    (loop for parser in parsers 
       append (funcall parser input))))

;;; =LET* is the natural syntax for lispers
(defmacro =let* (bindings &body body)
  "{LET*} for parsers. Returns a parser which =BINDs parsers in
BINDINGS and which then executes BODY with the results bound to vars
in BINDINGS. If var is {_} then the result is ignored. The last
expression in BODY has to return a parser which is finally applied to
input."
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
  "Returns a parser that applies PARSERS to input until a parser is
successful or fails."
  (declare (optimize (speed 3)))
  (labels ((non-consing-or (parsers)
	     (lambda (input)
	       (or (funcall (the function (first parsers)) input)
		   (when (rest parsers)
		     (funcall (the function
				(non-consing-or (rest parsers)))
			      input))))))
    (non-consing-or parsers)))

(defun =binary-and (parser-a parser-b)
  "Returns a parser which =BINDs PARSER-A and PARSER-B and fails if any
of those fails, otherwise it returns the result of PARSER-B."
  (=let* ((_ parser-a))
    parser-b))

(defun =and (&rest parsers)
  "Returns a parser which =BINDs PARSERS and which fails if any
parser fails, otherwise it returns the result of the last parser."
  (let ((p (first parsers))
	(ps (rest parsers)))
    (if ps
	(=binary-and p (apply #'=and ps))
	(or p (=result nil)))))

(defun =list (&rest parsers)
  "Returns a parser which =BINDs PARSERS and which fails if any
parser fails, otherwise it returns the results of PARSERS in a list."
  (if parsers 
      (=let* ((x (first parsers))
	      (xs (apply '=list (rest parsers))))
	(=result (cons x xs)))
      (=result nil)))

(defun =if (test-parser then-parser &optional (else-parser (=fail)))
  "Returns a parser which applies THEN-PARSER to input if applying
TEST-PARSER to input would succeed and which applies ELSE-PARSER
otherwise."
  (lambda (input)
    (if (funcall test-parser input)
	(funcall then-parser input)
	(funcall else-parser input))))

(defun =when (test-parser &rest parsers)
  "Returns a parser which =BINDs PARSERS as if by =AND if applying
TEST-PARSER to input would succeed, otherwise it fails."
  (=if test-parser (apply #'=and parsers)))

(defun =unless (test-parser &rest parsers)
  "Returns a parser which =BINDs PARSERS as if by =AND unless
applying TEST-PARSER to input would succeed, otherwise it fails."
  (=if test-parser (=fail) (apply #'=and parsers)))

(defun =not (parser)
  "Returns a parser which applies {(=ITEM)} to input unless PARSER
would succeed, otherwise it fails."
  (=if parser (=fail) (=item)))

(defun =prog1 (parser &rest parsers)
  "Returns a parser which =BINDs PARSER and PARSERS as if by =AND
but which returns the result of PARSER."
  (=let* ((result parser)
	  (_ (apply #'=and parsers)))
    (=result result)))

(defun =prog2 (parser1 parser2 &rest parsers)
  "Returns a parser which =BINDs PARSER1, PARSER2 and PARSERS as
if by =AND but which returns the result of PARSER2."
  (=and parser1 (apply #'=prog1 parser2 parsers)))

(defun =maybe (parser)
  "Returns a parser which applies PARSER to input and which will always
succeed."
  (=or parser (=result nil)))

(defun =satisfies (predicate)
  "Returns a parser which applies {(=ITEM)} to input and which returns
the result of {(=ITEM)} when it satisfies PREDICATE, otherwise it
fails."
  (=bind (=item)
	(lambda (x)
	  (if (funcall predicate x)
	      (=result x)
	      (=fail)))))

(defun =eql (x)
  "Returns a parser which applies {(=ITEM)} to input and which returns
the result of {(=ITEM)} when it is EQL to {X}, otherwise it fails."
  (=satisfies (lambda (y) (eql x y))))

(defun =one-of (list)
  "Returns a parser which applies {(=ITEM)} to input and which returns
the result of {(=ITEM)} when it is EQL to an item in LIST, otherwise
it fails."
  (=let* ((item (=item)))
    (if (find item list :test #'eql)
	(=result item)
	(=fail))))

(defun =none-of (list)
  "Returns a parser which applies {(=ITEM)} to input and which returns
the result of {(=ITEM)} when it is not EQL to any item in LIST,
otherwise it fails."
  (=not (=one-of list)))

(defun =range (from to &key (parser (=item)) (predicate 'char<=))
  "Returns a parser which applies PARSER to input and which returns the
result of PARSER when it matches the range defined by FROM, TO and
PREDICATE, otherwise it fails."
  (=let* ((char parser))
    (if (funcall predicate from char to)
	(=result char) 
	(=fail))))

(defun =one-or-more (parser)
  "Returns a parser which =BINDs PARSER successfully one or more times
and which returns its results in a list, otherwise it fails."
  (=let* ((x parser)
	  (y (=zero-or-more parser)))
    (=result (cons x y))))

(defun =zero-or-more (parser)
  "Returns a parser which =BINDs PARSER successfully zero or more
times and which returns its results in a list."
  (=maybe (=one-or-more parser)))

(defun =one-to (n parser)
  "Returns a parser which =BINDs PARSER successfully one to {N} times
and which returns its results in a list, otherwise it fails."
  (case n
    (0 (=result nil))
    (t (=let* ((x parser)
	       (xs (=or (=one-to (1- n) parser)
			(=result nil))))
	 (=result (cons x xs))))))

(defun =zero-to (n parser)
  "Returns a parser which =BINDs PARSER successfully one to {N} times
and which returns its results in a list."
  (=maybe (=one-to n parser)))

(defun =at-least (n parser &key limit)
  "Returns a parser which =BINDs PARSER successfully at least {N} but
no more than LIMIT times and which returns its results in a list,
otherwise it fails."
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
  "Returns a parser which =BINDs PARSER successfully exactly {N}
times and which returns its results in a list, otherwise it fails." 
  (=at-least n parser :limit n))

(defun =funcall (parser function)
  "Returns a parser which applies PARSER to input and which returns
FUNCTION called on PARSER's successful result, otherwise it fails."
  (=let* ((result parser))
    (if result
	(=result (funcall function result))
	(=fail))))

(defun run (parser input-source &key (result #'caar))
  "Runs a PARSER against INPUT-SOURCE, which can be an {array}, a
{stream} or a {list}. RESULT is then called on the resulting list of
 {(result . input)} pairs."
  (let ((*input-at-run* (make-input input-source)))
    (funcall result (funcall parser *input-at-run*))))
