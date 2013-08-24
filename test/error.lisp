;;;; Test error signaling framework.
;;;; TODO: test =RESTART-CASE

(defpackage mpc.error-test
  (:use :cl
        :mpc
        :mpc.characters)
  (:export :test-input-position
           :test-=handler-case))

(in-package :mpc.error-test)

(defun test-input-position ()
  "Test if GET-INPUT-POSITION works correctly during =FAIL."
  (run (=or (=and (=string "foo")
                  (=or (=string "bar")
                       (=fail (assert (= 3 (get-input-position))))))
            (=and (=string "fooza")
                  (=or (=string "bar")
                       (=fail (assert (= 5 (get-input-position)))))))
       "foozakar"))

(defun test-=handler-case ()
  "Test =HANDLER-CASE."
  (assert (= 42
             (run (=handler-case (=fail (error "foo"))
                                 (error () (=result 42)))
                  ""))))
