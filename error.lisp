;;;; Funtions and parsers that ease the handling of parser failures.

(in-package :mpc)

(defun parse-line-position (input position)
  "Parses line position of {POSITION} in {INPUT}."
  (loop for i from 0 to position
     for in = input then (input-rest in)
     for newline-p = (if (input-empty-p in) nil
			 (char= #\Newline (input-first in)))
     for character = 0 then (if newline-p 0 (1+ character))
     for line = 1 then (+ line (if newline-p 1 0))
     when (= i position) return (values line character)))

(defun get-input-position ()
  "Returns current position, line position and character position."
  (unless *input-during-fail*
    (error "GET-INPUT-POSITION may only be called inside a FAIL HANDLING
body or a =HANDLER-CASE error clause."))
  (let ((position (input-position *input-during-fail*)))
    (if (eq (input-element-type *input-during-fail*) 'character)
	(multiple-value-bind (line character)
	    (parse-line-position *input-at-run* position)
	  (values position line character))
	position)))

(defun cases-to-parser-cases (cases input)
  "Utility macro function for {=HANDLER-CASE} and {=RESTART-CASE}."
  (loop for case in cases collect
       `(,(first case)
	 ,(second case)
	  (funcall (=and ,@(cddr case)) ,input))))

(defmacro =handler-case (parser &rest handlers)
  "Returns a parser which establishes {HANDLERS} as if by {HANDLER-CASE}
and which applies {PARSER} to input."
  (let ((parser-name (gensym "PARSER"))
	(input (gensym "INPUT")))
    `(let ((,parser-name ,parser))
       (lambda (,input)
	 (let ((*input-during-fail* ,input))
	   (handler-case (funcall ,parser-name ,input)
	     ,@(cases-to-parser-cases handlers input)))))))

(defmacro =restart-case (parser &rest restarts)
  "Returns a parser which establishes {RESTARTS} as if by {RESTART-CASE}
and which applies {PARSER} to input."
  (let ((parser-name (gensym "PARSER"))
	(input (gensym "INPUT")))
    `(let ((,parser-name ,parser))
       (lambda (,input)
	 (restart-case (funcall ,parser-name ,input)
	   ,@(cases-to-parser-cases restarts input))))))
