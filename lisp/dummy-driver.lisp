;;;; dummy-driver.lisp
;;;;
;;;; Copyright (c) 2015 Andrey Smirnov & Robert Smith

(in-package #:cl-iio)

;;; Exmaple use of the library with the dummy driver. This file is
;;; more of a scratchpad than anything else.

(defvar *dummy-device* (find-device-by-name "iio_dummy_part_no" :refresh-cache t))

(defun initialize-dummy-device (&rest channels-to-enable)
  ;; Ensure the buffer is disabled so we can operate on the device.
  (disable-buffer *dummy-device*)
  
  ;; Set the length of the buffer to 10, whatever that means.
  (setf (buffer-length *dummy-device*) 10)
  
  ;; Ensure all of the channels are disabled.
  (map nil #'disable-channel (device-channels *dummy-device*))
  
  (dolist (channel-name channels-to-enable)
    (enable-channel (find-channel-by-name *dummy-device* channel-name)))
  
  ;; Enable the buffer.
  (enable-buffer *dummy-device*))

(defun test-dummy-device ()
  (initialize-dummy-device "accel_x" "timestamp" "voltage0" "voltage1-voltage2" "voltage3-voltage4")
  (with-open-file (s "/sys/bus/iio/devices/trigger0/trigger_now"
                     :direction ':output
                     :element-type 'character
                     :if-does-not-exist ':error)
    (write-line "derp" s))
  (with-open-device *dummy-device*
    (multiple-value-bind (raw bytes-read)
        (read-raw-record *dummy-device*)
      (format t "RAW: ~S~%" raw)
      (format t "BYTES READ: ~S~%" bytes-read)
      (format t "PARSED: ~S~%" (parse-raw-record *dummy-device* raw)))))
