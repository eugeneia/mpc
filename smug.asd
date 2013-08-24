;;;; System definition for smug.

(defpackage smug-asd
  (:documentation
   "System definition for SMUG and SMUG.CHARACTERS.")
  (:use :cl
	:asdf))

(in-package :smug-asd)

(defsystem smug
  :components ((:file "packages")
	       (:file "input" :depends-on ("packages"))
	       (:file "smug" :depends-on ("packages" "input"))
	       (:file "characters" :depends-on ("packages" "smug"))
	       (:file "numerals" :depends-on ("packages"
					      "smug"
					      "characters"))
	       (:file "error" :depends-on ("packages" "smug"))))
