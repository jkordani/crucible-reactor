;;;; crucible-reactor.asd

(asdf:defsystem #:crucible-reactor
  :description "Describe crucible-reactor here"
  :author "Joshua Kordani"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:drakma #:cl-json)
  :components ((:file "package")
               (:file "crucible-reactor")))
