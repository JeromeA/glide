(asdf:defsystem "glide"
  :description "List symbols of a given package"
  :version "0.0.1"
  :serial t
  :components ((:file "glide-package")
               (:file "symbol-info")
               (:file "package-info")))
