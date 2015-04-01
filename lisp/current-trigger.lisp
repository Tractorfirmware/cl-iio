;;;; current-trigger.lisp
;;;;
;;;; Copyright (c) 2015 Andrey Smirnov & Robert Smith

(in-package #:cl-iio)

(defun device-current-trigger-path (device)
  (merge-pathnames "trigger/current_trigger"
                   (device-pathname device)))

(defun null-out-current-trigger (device)
  (with-open-file (s (device-current-trigger-path device)
                     :direction ':output
                     :if-does-not-exist ':error)
    (write-line "NULL" s))
  nil)
