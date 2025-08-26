(in-package :glide)

(defun package-symbols (package-name)
  (let ((pkg (find-package package-name)))
    (when pkg
      (loop for symbol being the symbols of pkg
            collect symbol))))
