(defsystem "glide"
  :description "List symbols of a given package"
  :depends-on (#:trivial-gray-streams)
  :serial t
  :components ((:file "glide-package")
               (:file "symbol-info")
               (:file "package-info")
               (:file "server")
               (:file "t")))
