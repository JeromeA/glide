(in-package :glide)

(defvar *standard-error* *error-output*)
(defvar *real-standard-output* *standard-output*)

(defclass repl-stream (sb-gray:fundamental-character-output-stream)
  ((label :initarg :label)
   (id :initarg :id)))

(defmethod sb-gray:stream-write-string ((s repl-stream) string &optional (start 0) end)
  (let ((segment (if (and (= start 0) (null end))
                     string
                     (subseq string start end))))
    (format *real-standard-output* "(~a ~a ~S)~%"
            (slot-value s 'label)
            (slot-value s 'id)
            segment)))

(defmethod sb-gray:stream-write-char ((s repl-stream) char)
  (format *real-standard-output* "(~a ~a ~S)~%" (slot-value s 'label) (slot-value s 'id) (string char)))

(defun eval-and-capture (id form-string)
  (setf *real-standard-output* *standard-output*)
  (handler-case
      (let* ((out (make-instance 'repl-stream :label "stdout" :id id))
             (err (make-instance 'repl-stream :label "stderr" :id id))
             (*standard-output* out)
             (*standard-error* err)
             (*error-output* err))
        (multiple-value-bind (form /*remaining*/)
            (read-from-string form-string)
          (let ((res (cl:eval form)))
            (format *real-standard-output* "(result ~a ~S)~%" id res))))
    (error (e)
      (format *real-standard-output* "(error ~a ~S)~%" id (princ-to-string e)))))

(defun start-server ()
  (loop for form = (read *standard-input* nil :eof)
        until (eq form :eof)
        do (cl:eval form)))

