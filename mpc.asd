;;;; System definition for MPC.

(defsystem mpc
  :components ((:file "packages")
	       (:file "input"
                      :depends-on ("packages"))
	       (:file "mpc"
                      :depends-on ("packages"
                                   "input"))
	       (:file "characters"
                      :depends-on ("packages"
                                   "mpc"))
	       (:file "numerals"
                      :depends-on ("packages"
                                   "mpc"
                                   "characters"))
	       (:file "error"
                      :depends-on ("packages"
                                   "mpc"))))
