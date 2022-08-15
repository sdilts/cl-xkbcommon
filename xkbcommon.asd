(asdf:defsystem #:xkbcommon
  :description "FFI bindings for xkbcommon"
  :author "Stuart Dilts"
  :license "MIT"
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi #:cffi-grovel)
  :serial t
  :components ((:file "package")
	       (:cffi-grovel-file "xkbcommon-grovel")
	       (:file "xkbcommon")))
