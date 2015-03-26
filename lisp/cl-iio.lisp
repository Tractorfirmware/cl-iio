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
                       :direction ':input
                       :element-type 'character
                       :if-does-not-exist ':error)
      (write-line "1" s))))

(defgeneric disable-channel (channel)
  (:documentation "Disable the channel CHANNEL.")
  (:method ((channel channel))
    (with-open-file (s (channel-enabler-pathname channel)
                       :direction ':input
                       :element-type 'character
                       :if-does-not-exist ':error)
      (write-line "0" s))))

(defgeneric channel-enabled-p (channel)
  (:documentation "Is the channel CHANNEL enabled?")
  (:method ((channel channel))
    (with-open-file (s (channel-enabler-pathname channel)
                       :direction ':output
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
             :documentation "The device's channels.")
   (character-device-pathname :initarg :character-device-pathname
                              :accessor device-character-device-pathname
                              :documentation "The pathname of the associated character device for devices that support triggered buffer operations."))
  (:documentation "An Industrial IO device."))

(defmethod print-object ((device device) stream)
  (print-unreadable-object (device stream :type t :identity t)
    (prin1 (device-name device) stream)))

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
  "Find all the channels given the device pathname DEVICE-PATH. Return a list of CHANNEL objects."
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
                 (merge-pathnames filename channel-path))))
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
                               :byte-position shift)))))))

(defun device-from-device-path (device-path)
  "Create a DEVICE object given its device pathname DEVICE-PATH."
  (let ((assignment (first (last (pathname-directory device-path)))))
    (make-instance 'device
                   :pathname device-path
                   :assignment assignment
                   :name (name-from-device-path device-path)
                   :character-device-pathname (merge-pathnames assignment "/dev/")
                   :channels (channels-from-device-path device-path))))

(defun find-device-by-name (name-query)
  "Find a device named by the string NAME-QUERY and return a DEVICE object. If one wasn't found, return NIL."
  (dolist (device-path (device-paths))
    (when (string= name-query (name-from-device-path device-path))
      (return (device-from-device-path device-path)))))
