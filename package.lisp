(defpackage #:xkbcommon
  (:use :cffi :cl)
  (:nicknames #:xkb)
  (:export #:keysym-get-name ; functions
	   #:keysym-from-name
	   #:keysym-to-utf8
	   #:keycode-is-legal-ext-p
	   #:keycode-is-legal-x11-p
	   #:new-context
	   #:new-keymap-from-names
	   #:keymap-key-get-name
	   #:keymap-unref
	   #:context-unref
	   #:state-key-get-syms
	   #:state-key-get-one-sym
	   #:state-key-get-syms
	   ;; enums, structs, ect
	   #:mod-index
	   #:mod-mask
	   #:modifier-names
	   #:led-names
	   #:keycode
	   #:keysym
	   #:layout-index
	   #:layout-mask
	   #:level-index
	   #:led-index
	   #:invalid-masks
	   #:+keycode-max+
	   #:rule-names
	   #:keysym-flags
	   #:context-flags
	   #:compile-flags
	   #:state-component
	   #:state-match
	   #:consumed-mode
	   #:keymap
	   #:context
	   #:state
	   ;; conditions
	   #:context-creation-error
	   #:keymap-creation-error
	   ;; macros
	   #:with-keymap-from-names
	   #:with-xkb-context))
