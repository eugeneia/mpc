;;;; Input primitives.

(in-package :mpc)


;;; Input interface

(defgeneric make-input (source)
  (:documentation "Returns input object for {SOURCE}."))

(defgeneric input-position (input)
  (:documentation "Returns index position of {INPUT}."))

(defgeneric input-element-type (input)
  (:documentation "Returns element type of {INPUT}."))

(defgeneric input-empty-p (input)
  (:documentation "Predicate to test if {INPUT} is empty."))

(defgeneric input-first (input)
  (:documentation "Returns first element of {INPUT}."))

(defgeneric input-rest (input)
  (:documentation "Returns {INPUT} with its first element stripped."))


;;; Generic INPUT-ELEMENT-TYPE implementation

(defmethod input-element-type ((input t))
  nil)


;;; Generic index implementation

(deftype array-index ()
  "Array index type used in index structure."
  `(integer 0 ,array-dimension-limit))

(defstruct index
  "Generic index."
  (position
   0
   :type array-index
   :read-only t))

(defmethod input-position ((input index))
  (index-position input))


;;; Implementation for lists

(defstruct (index-list (:include index))
  "Index list."
  (list
   (error "Must supply LIST.")
   :type list
   :read-only t))

(defmethod make-input ((input list))
  (make-index-list :list input))

(defmethod input-empty-p ((input index-list))
 (declare (optimize (speed 3) (safety 0)))
  (not (index-list-list input)))

(defmethod input-first ((input index-list))
  (declare (optimize (speed 3) (safety 0)))
  (first (index-list-list input)))

(defmethod input-rest ((input index-list))
  (declare (optimize (speed 3)))
  (make-index-list :list (rest (index-list-list input))
		   :position (1+ (the array-index
				   (index-position input)))))


;;; Implementation for arrays

(defstruct (index-array (:include index))
  "Index array."
  (array
   (error "Must supply ARRAY.")
   :type array
   :read-only t))

(defstruct (index-simple-array (:include index-array))
  "Index simple array.")

(defstruct (index-simple-string (:include index-array))
  "Index simple string.")

(defmethod make-input ((input array))
  (etypecase input
    (simple-array (make-index-simple-array :array input))
    (array        (make-index-array :array input))))

(defmethod make-input ((input string))
  (etypecase input
    (simple-string (make-index-simple-string :array input))
    (string        (make-index-array :array input))))

(defmethod input-element-type ((input index-array))
  (array-element-type (index-array-array input)))

(defmethod input-empty-p ((input index-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (= (the array-index (index-position input))
     (the array-index (length (the array (index-array-array input))))))

(defmethod input-first ((input index-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the array (index-array-array input))
        (index-position input)))

(defmethod input-first ((input index-simple-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the simple-array (index-array-array input))
        (index-position input)))

(defmethod input-first ((input index-simple-string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the simple-string (index-array-array input))
        (index-position input)))

(defmethod input-rest ((input index-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-array :array (index-array-array input)
		    :position (1+ (the array-index
				    (index-position input)))))

(defmethod input-rest ((input index-simple-array))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-simple-array
   :array (index-array-array input)
   :position (1+ (the array-index
		   (index-position input)))))

(defmethod input-rest ((input index-simple-string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-simple-string :array (index-array-array input)
			    :position (1+ (the array-index
					    (index-position input)))))


;;; Implementation for streams

(defmethod make-input ((input file-stream))
  (let ((array (make-array (file-length input)
			   :element-type (stream-element-type input))))
    (make-input (subseq array 0 (read-sequence array input)))))

;;;

(defstruct (readline-stream (:include index))
  (read-function (lambda () (read-line t nil))
   :type function
   :read-only t)
  (string
   ""
   :type string))

(defmethod make-input ((input readline-stream))
  input)

(defmethod input-element-type ((input readline-stream))
  'character)

(defmethod input-empty-p ((input readline-stream))
  (loop
    (if (<= (length (readline-stream-string input))
	    (readline-stream-position input))
	(let ((line (funcall (readline-stream-read-function input))))
	  (if line
	      (setf (readline-stream-string input)
		    (concatenate 'string
				 (readline-stream-string input)
				 (string #\newline)
				 line))
	      (return t)))
	(return nil))))

(defmethod input-first ((input readline-stream))
  (aref (readline-stream-string input)
	(readline-stream-position input)))

(defmethod input-rest ((input readline-stream))
  (make-readline-stream
   :read-function (readline-stream-read-function input)
   :string (readline-stream-string input)
   :position (1+ (readline-stream-position input))))
