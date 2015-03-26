;;;; cl-iio.asd
;;;;
;;;; Copyright (c) 2015 Andrey Smirnov & Robert Smith

(asdf:defsystem #:cl-iio
  :description "An interface to the Industrial IO Linux subsystem"
  :author "Andrey Smirnov & Robert Smith"
  :license "BSD 3-clause (see LICENSE)"
  :serial t
  :components ((:file "package")
               (:file "cl-iio")))

