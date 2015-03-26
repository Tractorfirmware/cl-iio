;;;; cl-iio.lisp
;;;;
;;;; Copyright (c) 2015 Andrey Smirnov & Robert Smith

(in-package #:cl-iio)

(alexandria:define-constant +iio-directory+ (pathname "/sys/bus/iio/devices/")
  :test #'cl-fad:pathname-equal
  :documentation "The path to the list of all of the IIO devices.")

(defclass channel ()
  ((name :initarg :name
         :accessor channel-name
         :documentation "The name of a device channel.")
   (enabler-pathname :initarg :enabler-pathname
                     :accessor channel-enabler-pathname
                     :documentation "The pathname to the writable file which enables a channel.")
   (type-pathname :initarg :type-pathname
                  :accessor channel-type-pathname
                  :documentation "The pathname of the readable file which contains the type of information. This information is parsed and stored in a CHANNEL object as well.")
   (index-pathname :initarg :index-pathname
                   :accessor channel-index-pathname
                   :documentation "The pathname of the readable file which contains the index of the channel. This information is parsed and stored in the CHANNEL object as well, specifically the INDEX slot.")
   (unparsed-type :initarg :unparsed-type
                  :accessor channel-unparsed-type
                  :documentation "The type of the channel as stated by the Industrial IO subsystem.")
   (index :initarg :index
          :accessor channel-index
          :documentation "The channel's index.")
   (endianness :initarg :endianness
               :accessor channel-endianness
               :type (member :big :little)
               :documentation "The endianness of the channel's data.")
   (signedness :initarg :signedness
               :accessor channel-signedness
               :type (member :signed :unsigned)
               :documentation "The sign of the channel's data.")
   (data-length :initarg :data-length
                :accessor channel-data-length
                :documentation "The total number of bits in the channel's data.")
   (byte-length :initarg :byte-length
                :accessor channel-byte-length
                :documentation "The number of relevant bits in the channel's data.")
   (byte-position :initarg :byte-position
                  :accessor channel-byte-position
                  :documentation "The position (from the LSB) of the start of the relevant bits in the channel's data."))
  (:documentation "The description of a channel."))

(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)
    (format stream "~S index=~D type=~A"
            (channel-name channel)
            (channel-index channel)
            (channel-unparsed-type channel))))

(defgeneric enable-channel (channel)
  (:documentation "Enable the channel CHANNEL.")
  (:method ((channel channel))
    (with-open-file (s (channel-enabler-pathname channel)
                       :direction ':output
                       :element-type 'character
                       :if-does-not-exist ':error)
      (write-line "1" s))))

(defgeneric disable-channel (channel)
  (:documentation "Disable the channel CHANNEL.")
  (:method ((channel channel))
    (with-open-file (s (channel-enabler-pathname channel)
                       :direction ':output
                       :element-type 'character
                       :if-does-not-exist ':error)
      (write-line "0" s))))

(defgeneric channel-enabled-p (channel)
  (:documentation "Is the channel CHANNEL enabled?")
  (:method ((channel channel))
    (with-open-file (s (channel-enabler-pathname channel)
                       :direction ':input
                       :element-type 'character
                       :if-does-not-exist ':error)
      (char/= #\0 (read-char s t)))))

(defclass device ()
  ((pathname :initarg :pathname
             :accessor device-pathname
             :documentation "The pathname of the device.")
   (assignment :initarg :assignment
               :accessor device-assignment
               :documentation "The assigned device name by Linux.")
   (name :initarg :name
         :accessor device-name
         :documentation "The name of the device as specified by the driver.")
   (channels :initarg :channels
             :accessor device-channels
             :documentation "The vector of the device's channels. The index of each channel in the vector corresponds to the channel's index (i.e., CHANNEL-INDEX).")
   (character-device-pathname :initarg :character-device-pathname
                              :accessor device-character-device-pathname
                              :documentation "The pathname of the associated character device for devices that support triggered buffer operations.")
   (stream :accessor device-stream
           :documentation "The buffered stream."))
  (:documentation "An Industrial IO device."))

(defmethod print-object ((device device) stream)
  (print-unreadable-object (device stream :type t :identity t)
    (prin1 (device-name device) stream)))

(defgeneric open-device (device)
  (:documentation "Open the device DEVICE for reading.")
  (:method ((device device))
    (setf (device-stream device)
          (open (device-character-device-pathname device)
                :direction ':output
                :element-type '(unsigned-byte 8)
                :if-does-not-exist ':error))))

(defgeneric close-device (device)
  (:documentation "Close the device DEVICE.")
  (:method ((device device))
    (close (device-stream device))
    (setf (device-stream device) nil)))

(defmacro with-open-device (device &body body)
  "Execute BODYwith the device DEVICE opened, closing it afterward."
  `(unwind-protect
        (progn
          (open-device device)
          ,@body)
     (close-device device)))

(defun device-paths ()
  "List all of the device paths."
  (flet ((valid-device-pathname-p (pathname)
           (let ((valid-prefix-1 "iio:device")
                 (valid-prefix-2 "iio\\:device"))
             (and (cl-fad:directory-pathname-p pathname)
                  (let ((name (final-directory pathname)))
                    (or (string= valid-prefix-1 name
                                 :end2 (min (length name) (length valid-prefix-1)))
                        (string= valid-prefix-2 name
                                 :end2 (min (length name) (length valid-prefix-2)))))))))
    (remove-if (complement #'valid-device-pathname-p)
               (cl-fad:list-directory +iio-directory+))))

(defun name-from-device-path (device-path)
  "Read the name of a device from its device pathname DEVICE-PATH."
  (read-one-line (merge-pathnames "name" device-path)))

(defun parse-channel-type-string (type-string)
  "Parse a channel type string TYPE-STRING. Return 5 values:

    1. Endianness: :LITTLE or :BIG
    2. Signedness: :SIGNED or :UNSIGNED
    3. The number of bits used: a positive integer
    4. The total number of bits: a positive integer
    5. The location (shift) of the relevant bits: a positive integer"
  ;; This function assumes it is formatted correctly. Beware!
  (let* (
         ;; First, find the positions of the separator characters.
         (colon (position #\: type-string :test #'char=))
         (slash (position #\/ type-string :test #'char=))
         (shift-sign (position #\> type-string :test #'char=))
         
         ;; Now, find the unparsed components of the type string.
         (endianness (subseq type-string 0 colon))
         (signedness (char type-string (1+ colon)))
         (bits-used (subseq type-string (+ 2 colon) slash))
         (bits-total (subseq type-string (1+ slash) shift-sign))
         ;; Note that we need to skip the second '>', hence +2 instead
         ;; of +1.
         (shift-amount (subseq type-string (+ 2 shift-sign))))
    (values
     ;; Endianness
     (cond ((string-equal "le" endianness) :little)
           ((string-equal "be" endianness) :big)
           (t (error "Unknown endianness ~S" endianness)))
     ;; Signedness
     (ecase signedness
       ((#\s) :signed)
       ((#\u) :unsigned))
     ;; Bits Used
     (parse-integer bits-used)
     ;; Total number of bits
     (parse-integer bits-total)
     ;; Shift amount
     (parse-integer shift-amount))))

(defun channels-from-device-path (device-path)
  "Find all the channels given the device pathname DEVICE-PATH. Return a vector of CHANNEL objects."
  ;; FIXME: This function is only aware of in_* channels.
  (let ((channel-path (merge-pathnames "scan_elements/" device-path)))
    (labels ((channel-name-from-file (file)
               (multiple-value-bind (prefix root suffix)
                   (dismantle-string (pathname-name file))
                 (declare (ignore prefix suffix))
                 root))
             (enabler-pathname (name)
               (let ((filename (concatenate 'string "in_" name "_en")))
                 (merge-pathnames filename channel-path)))
             (type-pathname (name)
               (let ((filename (concatenate 'string "in_" name "_type")))
                 (merge-pathnames filename channel-path)))
             (index-pathname (name)
               (let ((filename (concatenate 'string "in_" name "_index")))
                 (merge-pathnames filename channel-path)))
             (sort-channels (channels)
               (sort channels #'< :key #'channel-index)))
      (let* ((channel-files (cl-fad:list-directory channel-path))
             (channel-names (delete-duplicates
                             (mapcar #'channel-name-from-file channel-files)
                             :test #'string=)))
        (loop :for name :in channel-names
              :for type-pathname := (type-pathname name)
              :for index-pathname := (index-pathname name)
              :for type-string := (read-one-line type-pathname)
              :collect
              (multiple-value-bind (endianness signedness bits-used bits-total shift)
                  (parse-channel-type-string type-string)
                (make-instance 'channel
                               :name name
                               :enabler-pathname (enabler-pathname name)
                               :type-pathname type-pathname
                               :index-pathname index-pathname
                               :index (parse-integer (read-one-line index-pathname))
                               :unparsed-type type-string
                               :endianness endianness
                               :signedness signedness
                               :data-length bits-total
                               :byte-length bits-used
                               :byte-position shift))
                :into channels
              :finally (return (sort-channels (coerce channels 'vector))))))))

(defun device-from-device-path (device-path)
  "Create a DEVICE object given its device pathname DEVICE-PATH."
  (let ((assignment (first (last (pathname-directory device-path)))))
    (make-instance 'device
                   :pathname device-path
                   :assignment assignment
                   :name (name-from-device-path device-path)
                   :character-device-pathname (merge-pathnames assignment "/dev/")
                   :channels (channels-from-device-path device-path))))

(defvar *device-cache* (make-hash-table :test 'equal)
  "A cache of device objects. The table maps the device name to its device object.")

(defun find-device-by-name (name-query &key (refresh-cache nil))
  "Find a device named by the string NAME-QUERY and return a DEVICE object. If one wasn't found, return NIL.

If REFRESH-CACHE is T, then the cache entry will be reset for the device that's found."
  (let ((maybe-object (gethash name-query *device-cache*)))
    (if (not (or refresh-cache
                 (null maybe-object)))
        maybe-object
        (dolist (device-path (device-paths))
          (when (string= name-query (name-from-device-path device-path))
            (return (setf (gethash name-query *device-cache*)
                          (device-from-device-path device-path))))))))

(defun find-channel-by-name (device name-query)
  "Find the channel whose name is NAME-QUERY in the device DEVICE. If one couldn't be found, return NIL."
  (find name-query (device-channels device)
        :test #'string=
        :key #'channel-name))

(defun device-record-length (device)
  "Compute the expected length (in bytes) of a record to be read from the device DEVICE. The record length depends on the channels which have been enabled."
  (loop :for channel :across (device-channels device)
        :when (channel-enabled-p channel)
          :sum (channel-data-length channel) :into bit-length
        :finally (return (ceiling bit-length 8))))

(defun read-raw-record (device &key buffer record-length)
  "Read a record from an opened device DEVICE.

If BUFFER is specified, then it will be filled. If not, one will be allocated.

If RECORD-LENGTH is specified, then it will be assumed that RECORD-LENGTH bytes comprises a record. If it is not specified, then it will be calculated."
  (assert (not (null (device-stream device)))
          (device)
          "The device ~S is not open and it must be in order to read from it."
          device)
  (when (null record-length)
    (setf record-length (device-record-length device)))
  (when (null buffer)
    (setf buffer (make-array record-length :element-type '(unsigned-byte 8)
                                           :initial-element 0)))
  (read-sequence buffer (device-stream device))
  buffer)

(defun parse-raw-record (device record)
  "Parse the record RECORD (an unsigned octet array) according to the device DEVICE.

The output will be a vector of integer values corresponding to each channel."
  (let ((byte-index 0)
        (parsed-record (make-array (length (device-channels device))
                                   :element-type 'integer
                                   :initial-element 0)))
    (loop :for channel :across (device-channels device)
          :for bytes-to-read := (channel-data-length channel)
          ;; XXX: We really shouldn't check whether a channel is
          ;; enabled for every run. This could be calculated and
          ;; cached.
          :when (channel-enabled-p channel)
            :do (setf (aref parsed-record (channel-index channel))
                      (ecase (channel-endianness channel)
                        ((:big)
                         (loop
                           :for idx :from byte-index
                             :below (+ byte-index bytes-to-read)
                           :for byte := (aref record idx)
                             :then (logand (aref record idx)
                                           (ash byte 8))
                           :finally (return (ash byte (- (channel-byte-position channel))))))
                        ((:little)
                         (loop
                           :for idx :from (1- (+ byte-index bytes-to-read))
                           :downto byte-index
                           :for byte := (aref record idx)
                             :then (logand (aref record idx)
                                           (ash byte 8))
                           :finally (return (ash byte (- (channel-byte-position channel)))))))
                      (byte-index (+ byte-index bytes-to-read)))
          :finally (return parsed-record))))
