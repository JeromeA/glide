(in-package :glide)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :trivial-gray-streams))

(defvar *standard-error* *error-output*)
(defvar *real-standard-output* *standard-output*)

(defclass repl-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((label :initarg :label)))

(defmethod trivial-gray-streams:stream-write-string ((s repl-stream) string start end)
  (declare (ignore start end))
  (format *real-standard-output* "(~a ~S)~%" (slot-value s 'label) string))

(defmethod trivial-gray-streams:stream-write-char ((s repl-stream) char)
  (format *real-standard-output* "(~a ~S)~%" (slot-value s 'label) (string char)))

(defun eval (form)
  (setf *real-standard-output* *standard-output*)
  (handler-case
      (let* ((out (make-instance 'repl-stream :label "stdout"))
             (err (make-instance 'repl-stream :label "stderr"))
             (*standard-output* out)
             (*standard-error* err)
             (*error-output* err))
        (let ((res (cl:eval form)))
          (format *real-standard-output* "(result ~S)~%" res)))
    (error (e)
      (format *real-standard-output* "(error ~S)~%" (princ-to-string e)))))

(defun start-server ()
  (loop for form = (read *standard-input* nil :eof)
        until (eq form :eof)
        do (eval form)))

