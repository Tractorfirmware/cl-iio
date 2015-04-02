;;;; cl-iio.asd
;;;;
;;;; Copyright (c) 2015 Andrey Smirnov & Robert Smith

(asdf:defsystem #:cl-iio
  :description "An interface to the Industrial IO Linux subsystem"
  :author "Andrey Smirnov & Robert Smith"
  :license "BSD 3-clause (see LICENSE)"
  :depends-on (#:alexandria #:cl-fad)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "cl-iio")
               (:file "trigger")))

