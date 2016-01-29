;;;; Monadic parser primitives and utility parsers.

(in-package :mpc)

(defvar *input-at-run* nil
  "Bound to initial input during RUN.")

(defvar *input-during-fail* nil
  "Bound to input during =FAIL.")

(defun =end-of-input ()
  "*Description:*

   {=end-of-input} succeeds only if the input is empty."
  (declare (optimize (speed 3)))
  (lambda (input)
    (when (input-empty-p input)
      (list (cons t input)))))

(defun =result (value)
  "*Arguments and Values:*

   _value_—an _object_.

   *Description:*

   {=result} always succeeds with _value_ as its result."
  (declare (optimize (speed 3)))
  (lambda (input)
    (list (cons value input))))

(defmacro =fail (&body handling)
  "*Arguments and Values:*

   _handling_—_forms_ to run when the parser is invoked.

   *Description:*

   {=fail} always fails. If supplied, the _handling forms_ will be run
   when {=fail} is applied. The _handling forms_ may call
   {get-input-position}."
  (if handling
      (let ((input (gensym "INPUT")))
	`(lambda (,input)
	   (let ((*input-during-fail* ,input))
	     ,@handling)
	   nil))
      '(constantly nil)))

(defun =item ()
  "*Description:*

   {=item} consumes the next item and succeeds with that item as its
   result unless the input is empty. Fails if the input is empty."
  (lambda (input)
    (unless (input-empty-p input)
      (list (cons (input-first input)
		  (input-rest input))))))

(defun =bind (parser make-parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   _make-parser_—a _function designator_ for a one-argument _function_
   which returns a _parser_.

   *Description:*

   {=bind} applies _parser_ to the input. For each resulting {(value
   . input)} pair {=bind} calls _make-parser_ with each _value_ and
   applies the resulting parser to each _input_. {=bind} succeeds with
   the concatenated results or fails if _parser_ fails."
  (declare (optimize (speed 3)))
  (lambda (input)
    (loop for (value . input) in (funcall parser input)
       append (funcall (funcall make-parser value) input))))

(defun =plus (&rest parsers)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {=plus} is the non-deterministic choice combinator. It applies
   _parsers_ to input and succeeds with the result of every successful
   _parser_. {=plus} fails if every _parser_ fails."
  (lambda (input)
    (loop for parser in parsers 
       append (funcall parser input))))

;;; =LET* is the natural syntax for lispers
(defmacro =let* (bindings &body forms)
  "_bindings_::= {(}{{}{(}_symbol_ _parser_{)}{\\}}\\*{)}

   *Arguments and Values:*

   _forms_—_forms_ of which the last _form_ must return a _parser_.

   _symbol_—a _symbol_.

   _parser_—a _form_ whose result is a _parser_.

   *Description:*

   {=let*} applies the _parsers_ in _bindings_ as if by {=and} and
   evaluates _forms_ in an implicit {let} environment where the results
   are bound to the _symbols_ in _bindings_. Finally, {=let*} applies the
   parser returned by the last _form_."
  (if bindings
      (let ((symbol (first (first bindings))))
	`(=bind ,@(cdr (first bindings))
                (lambda (,symbol)
                  ,@(when (string-equal (symbol-name symbol) "_")
                          `((declare (ignore ,symbol))))
                  (=let* ,(cdr bindings)
                    ,@forms))))
      `(progn ,@forms)))

(defun =or (&rest parsers)
  "*Arguments and Values:*

   _parsers_—_parsers_.

   *Description:*

   {=or} applies _parsers_ until one _parser_ succeeds, in which case it
   succeeds with the result of that _parser_. If no _parser_ succeeds
   {=or} fails."
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
  "*Arguments and Values:*

   _parsers_—_parsers_.

   *Description:*

   {=and} applies _parsers_ sequentially. If all _parsers_ succeed,
   {=and} succeeds with the last _parser_'s result. Otherwise {=and}
   fails."
  (let ((p (first parsers))
	(ps (rest parsers)))
    (if ps
	(=binary-and p (apply '=and ps))
	(or p (=result nil)))))

(defun =list (&rest parsers)
  "*Arguments and Values:*

   _parsers_—_parsers_.

   *Description:*

   {=list} applies _parsers_ sequentially. If all _parsers_ succeed,
   {=list} succeeds with a list of their results. Otherwise {=list}
   fails."
  (if parsers 
      (=let* ((x (first parsers))
	      (xs (apply '=list (rest parsers))))
	(=result (cons x xs)))
      (=result nil)))

(defun =if (test-parser then-parser &optional (else-parser (=fail)))
  "*Arguments and Values:*

   _test-parser_—a _parser_.

   _then-parser_—a _parser_.

   _else-parser_—a _parser_. The default is {(=fail)}.

   *Description:*

   {=if} applies _then-parser_ if _test-parser_ would succeed and
   _else-parser_ otherwise. Note that _test-parser_ is not actually
   applied to the input."
  (lambda (input)
    (if (funcall test-parser input)
	(funcall then-parser input)
	(funcall else-parser input))))

(defun =when (test-parser &rest parsers)
  "*Arguments and Values:*

   _test-parser_—a _parser_.

   _parsers_—_parsers_.

   *Description:*

   {=when} applies _parsers_ as if by {=and} if _test-parser_ would
   succeed. Note that _test-parser_ is not actually applied to the
   input."
  (=if test-parser (apply '=and parsers)))

(defun =unless (test-parser &rest parsers)
  "*Arguments and Values:*

   _test-parser_—a _parser_.

   _parsers_—_parsers_.

   *Description:*

   {=unless} applies _parsers_ as if by {=and} if _test-parser_ would
   fail. Note that _test-parser_ is not actually applied to the
   input."
  (=if test-parser (=fail) (apply '=and parsers)))

(defun =not (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {=not} consumes the next item and succeeds with that item as its
   result if _parser_ would fail. E.g. it negates _parser_. Note that
   _parser_ is not actually applied to the input. Fails if the input is
   empty."
  (=if parser (=fail) (=item)))

(defun =prog1 (parser &rest parsers)
  "*Arguments and Values:*

   _parser_—a _parser_.

   _parsers_—_parsers_.

   *Description:*

   {=prog1} applies _parser_ and _parsers_ sequentially. If they all
   succeed, {=prog1} succeeds with _parser_'s result. Otherwise {=prog1}
   fails."
  (=let* ((result parser)
	  (_ (apply '=and parsers)))
    (=result result)))

(defun =prog2 (parser1 parser2 &rest parsers)
  "*Arguments and Values:*

   _parser1_—a _parser_.

   _parser2_—a _parser_.

   _parsers_—_parsers_.

   *Description:*

   {=prog2} applies _parser1_, _parser2_ and _parsers_ sequentially. If
   they all succeed, {=prog2} succeeds with _parser2_'s result. Otherwise
   {=prog2} fails."
  (=and parser1 (apply '=prog1 parser2 parsers)))

(defun =maybe (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {=maybe} applies _parser_. If _parser_ succeeds {=maybe} will succeed
   with its result, otherwise it will succeed with {nil}."
  (=or parser (=result nil)))

(defun =satisfies (predicate)
  "*Arguments and Values:*

   _predicate_—a _function designator_ for a one-argument predicate
   _function_.

   *Description:*

   {=satisfies} consumes the next item and succeeds with that item as its
   result if the result satisfies _predicate_. Fails if the input is
   empty."
  (=bind (=item)
	(lambda (x)
	  (if (funcall predicate x)
	      (=result x)
	      (=fail)))))

(defun =eql (x)
  "*Arguments and Values:*

   _x_—an _object_.

   *Description:*

   {=eql} consumes the next item and succeeds with that item as its
   result if the item is {eql} to _object_. Fails if the input is empty."
  (=satisfies (lambda (y) (eql x y))))

(defun =one-of (list)
  "*Arguments and Values:*

   _list_—a _list_ of _objects_.

   *Description:*

   {=one-of} consumes the next item and succeeds with that item as its
   result if the item is {eql} to any _object_ in _list_. Fails if the
   input is empty."
  (=let* ((item (=item)))
    (if (find item list :test 'eql)
	(=result item)
	(=fail))))

(defun =none-of (list)
  "*Arguments and Values:*

   _list_—a _list_ of _objects_.

   *Description:*

   {=none-of} consumes the next item and succeeds with that item as its
   result unless the item is {eql} to one of the _objects_ in _list_.
   Fails if the input is empty."
  (=not (=one-of list)))

(defun =range (from to &key (parser (=item)) (predicate 'char<=))
  "*Arguments and Values:*

   _from_—an _object_.

   _to_—an _object_.

   _parser_—a _parser_. The default is {(=item)}.

   _predicate_—a _function designator_ for a three-argument predicate
   _function_. The default is {char<=}.

   *Description:*

   {=range} applies _parser_ and, if it succeeds, applies _predicate_ to
   _from_, its results and _to_. {=range} succeeds with the result of
   _parser_ if _predicate_ is _true_ and fails otherwise. Fails if the
   input is empty."
  (=let* ((char parser))
    (if (funcall predicate from char to)
	(=result char) 
	(=fail))))

(defun =one-or-more (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {=one-or-more} applies _parser_ repeatedly until it fails and succeeds
   with a list of the results if _parser_ succeeded at least one time."
  (=let* ((x parser)
	  (y (=zero-or-more parser)))
    (=result (cons x y))))

(defun =zero-or-more (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {=zero-or-more} applies _parser_ repeatedly until it fails and
   succeeds with a list of the results."
  (=maybe (=one-or-more parser)))

(defun =one-to (n parser)
  "*Arguments and Values:*

   _n_—a positive _integer_.

   _parser_—a _parser_.

   *Description:*

   {=one-to} applies _parser_ repeatedly until it fails and succeeds with
   a list of the results unless _parser_ succeeded less than once or more
   than _n_ times."
  (check-type n (integer 1 *))
  (=let* ((x parser)
          (xs (=or (=one-to (1- n) parser)
                   (=result nil))))
    (=result (cons x xs))))

(defun =zero-to (n parser)
  "*Arguments and Values:*

   _n_—an non-negative _integer_.

   _parser_—a _parser_.

   *Description:*

   {=zero-to} applies _parser_ repeatedly until it fails and succeeds
   with a list of the results unless _parser_ succeeded more than _n_
   times."
  (=if parser
       (=one-to n parser)
       (=result nil)))

(defun =at-least (n parser &key limit)
  "*Arguments and Values:*

   _n_, _limit_—_bounding index designators_. The default for _limit_ is
   {nil}.

   _parser_—a _parser_.

   *Description:*

   {=at-least} applies _parser_ repeatedly until it fails and succeeds
   with a list of the results unless _parser_ succeeded less than _n_
   times or, if _limit_ is not {nil}, more than _limit_ times."
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
  "*Arguments and Values:*

   _n_—an non-negative _integer_.

   _parser_—a _parser_.

   *Description:*

   {=exactly} applies _parser_ repeatedly until it fails and succeeds
   with a list of the results unless _parser_ succeeded not exactly _n_
   times."
  (=at-least n parser :limit n))

(defun =funcall (parser function)
  "*Arguments and Values:*

   _parser_—a _parser_.

   _function_—a _function designator_.

   *Description:*

   {=funcall} applies _parser_. If successful, {=funcall} applies
   _function_ on its result and succeeds with the return value."
  (=let* ((result parser))
    (if result
	(=result (funcall function result))
	(=fail))))

(defun run (parser input-source &key (result 'caar))
  "*Arguments and Values:*

   _parser_—a _parser_.

   _input-source_—an _array_, an _input stream_ or a _list_.

   _result_—a _function designator_ to a one-argument _function_. The
   default is {caar}.

   *Description:*

   {run} applies _parser_ to _input-source_ and returns the result of
   calling the _result_ function on the resulting list of
   {(value . input)} pairs."
  (let ((*input-at-run* (make-input input-source)))
    (funcall result (funcall parser *input-at-run*))))
