;;;; System definition for MPC.

(defsystem mpc
  :description
"Monadic Parser Combinators for Common Lisp. MPC tries to be simple and
practical while being powerful, well documented and fairly performant. A
friendly fork of Drew Crampsies _Smug_ library."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU Affero General Public License"
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
