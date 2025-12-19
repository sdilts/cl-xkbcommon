(asdf:defsystem #:xkbcommon
  :description "FFI bindings for xkbcommon"
  :author "Stuart Dilts"
  :license "MIT"
  :version "0.1.0"
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi #:cffi-grovel)
  :serial t
  :components ((:file "package")
	       (:cffi-grovel-file "xkbcommon-grovel")
	       (:file "xkbcommon")))
