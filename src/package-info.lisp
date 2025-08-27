(in-package :glide)

(defun package-info (package-name)
  (let ((pkg (find-package package-name)))
    (when pkg
      (list :name (package-name pkg)
            :description (documentation pkg t)
            :nicknames (package-nicknames pkg)
            :uses (mapcar #'package-name (package-use-list pkg))
            :exports (loop for sym being the external-symbols of pkg
                           collect (symbol-name sym))
            :shadows (mapcar #'symbol-name (package-shadowing-symbols pkg))
            :import-from (loop for sym being the present-symbols of pkg
                               for home = (symbol-package sym)
                               unless (eq pkg home)
                               collect (list (symbol-name sym)
                                             (package-name home)))))))
