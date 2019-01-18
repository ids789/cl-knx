;;;; cl-knx.asd

(asdf:defsystem #:cl-knx
  :description "A lisp interface to KNXD"
  :author "Ian Schipper <ids1@outlook.com>"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (:cffi :cl-autowrap)
  :components ((:module :spec)
			   (:file "package")
               (:file "cl-knx")))
