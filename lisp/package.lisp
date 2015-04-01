;;;; package.lisp
;;;;
;;;; Copyright (c) 2015 Andrey Smirnov & Robert Smith

(defpackage #:cl-iio
  (:use #:cl)
  ;; cl-iio.lisp
  (:export
   #:channel                            ; CLASS
   #:enable-channel                     ; GENERIC, METHOD
   #:disable-channel                    ; GENERIC, METHOD
   #:channel-enabled-p                  ; GENERIC, METHOD

   #:device                             ; CLASS
   #:device-channels                    ; ACCESSOR
   #:enable-buffer                      ; GENERIC, METHOD
   #:disable-buffer                     ; GENERIC, METHOD
   #:buffer-enabled-p                   ; GENERIC, METHOD
   #:buffer-length                      ; GENERIC, METHOD, SETF
   #:set-buffer-length                  ; GENERIC, METHOD
   #:open-device                        ; GENERIC, METHOD
   #:close-device                       ; GENERIC, METHOD
   #:with-open-device                   ; MACRO

   #:reset-device-cache                 ; FUNCTION

   #:list-devices                       ; FUNCTION
   #:find-device-by-name                ; FUNCTION
   #:find-channel-by-name               ; FUNCTION

   #:read-raw-record                    ; FUNCTION
   #:parse-raw-record                   ; FUNCTION
   )
  )
