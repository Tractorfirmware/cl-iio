;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2015 Andrey Smirnov & Robert Smith

(in-package #:cl-iio)

(defun final-directory (pathname)
  "The tail directory of a pathname PATHNAME."
  (if (not (cl-fad:directory-pathname-p pathname))
      nil
      (first (last (pathname-directory pathname)))))

(defun read-one-line (pathname)
  "Read and return the first line of a file designated by the pathname PATHNAME."
  (with-open-file (s pathname :direction ':input
                              :if-does-not-exist ':error)
    (read-line s t)))

(defun dismantle-string (string &key (separator #\_))
  "Dismantle a string into its prefix, root, and suffix. These components are separated by the character SEPARATOR, which is an underscore by default. Return NIL if the string doesn't have valid such components. Return the prefix, root, and suffix as three values otherwise."
  (let ((underscores (count separator string :test #'char=))
        (first-underscore (position separator string :test #'char=))
        (last-underscore (position separator string :test #'char= :from-end t)))
    (if (< underscores 2)
        nil
        (values
         ;; prefix
         (subseq string 0 first-underscore)
         ;; root
         (subseq string (1+ first-underscore) last-underscore)
         ;; suffix
         (subseq string (1+ last-underscore))))))
