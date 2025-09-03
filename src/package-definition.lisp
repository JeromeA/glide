(in-package :glide)

(defun package-definition (package-name)
  (let ((package (find-package package-name)))
    (when package
      (let ((imports (make-hash-table :test 'equal)))
        (loop for symbol being the present-symbols of package
              for home-package = (symbol-package symbol)
              unless (eq package home-package)
                do (push (symbol-name symbol)
                         (gethash (package-name home-package) imports)))
        `(defpackage ,(package-name package)
           ,@(let ((nicknames (package-nicknames package)))
               (when nicknames
                 `((:nicknames ,@nicknames))))
           ,@(let ((uses (package-use-list package)))
               (when uses
                 `((:use ,@(mapcar #'package-name uses)))))
           ,@(let ((shadows (package-shadowing-symbols package)))
               (when shadows
                 `((:shadow ,@(mapcar #'symbol-name shadows)))))
           ,@(let ((forms '()))
               (maphash (lambda (from-package symbols)
                          (push (list* :import-from from-package
                                       (nreverse symbols))
                                forms))
                        imports)
               (nreverse forms))
           ,@(let ((exports
                    (loop for symbol being the external-symbols of package
                          collect (symbol-name symbol))))
               (when exports
                 `((:export ,@exports))))
           ,@(let ((description (documentation package t)))
               (when description
                 `((:documentation ,description)))))))))
