(in-package :glide)

(defvar *standard-error* *error-output*)
(defvar *real-standard-output* *standard-output*)

(defclass repl-stream (sb-gray:fundamental-character-output-stream)
  ((label :initarg :label)))

(defmethod sb-gray:stream-write-string ((s repl-stream) string &optional start end)
  (format *real-standard-output* "(~a ~S)~%" (slot-value s 'label) string))

(defmethod sb-gray:stream-write-char ((s repl-stream) char)
  (format *real-standard-output* "(~a ~S)~%" (slot-value s 'label) (string char)))

(defun glide-eval (form-string)
  (setf *real-standard-output* *standard-output*)
  (handler-case
      (let* ((out (make-instance 'repl-stream :label "stdout"))
             (err (make-instance 'repl-stream :label "stderr"))
             (*standard-output* out)
             (*standard-error* err)
             (*error-output* err))
        (multiple-value-bind (form /*remaining*/)
            (read-from-string form-string)
          (let ((res (cl:eval form)))
            (format *real-standard-output* "(result ~S)~%" res))))
    (error (e)
      (format *real-standard-output* "(error ~S)~%" (princ-to-string e)))))

(defun start-server ()
  (loop for form = (read *standard-input* nil :eof)
        until (eq form :eof)
        do (cl:eval form)))

