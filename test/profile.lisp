(in-package :smug)

#-sbcl (error "This is SBCL code, sorry!")

(defun profile-call (function)
  ;; Don't accumulate results between runs.
  (sb-profile:reset)
  ;; Calling this every time through in case any of the user-defined
  ;; functions was recompiled.
  (sb-profile:profile
   make-input input-first input-rest input-empty-p index-position
   end-of-input result fail item bind =satisfies plus =or =and =if =prog1
   =prog2)
  (funcall function)
  (sb-profile:report))
