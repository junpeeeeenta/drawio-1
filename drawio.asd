(defpackage :drawio-system
  (:use :common-lisp :asdf))

(in-package :drawio-system)

(asdf:defsystem :drawio
  :name "drawio"
  :author "Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.0.1"
  :maintainer "Seiji Koide <koide@ontolonomy.co.jp>"
  :licence "MIT"
  :depends-on ("xmlreader" "line-reader")
  :serial t
  :components ((:file "drawioreader")
               ))
