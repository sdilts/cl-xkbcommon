(in-package #:xkb)

(define-foreign-library libxkbcommon
  (:unix (:or "libxkbcommon.so.0" "libxkbcommon"))
  (t (:default "libxkbcommon")))

(use-foreign-library libxkbcommon)

;; errors for when things aren't created properly:
(define-condition context-creation-error (error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Could not create the xkb context"))))

(define-condition keymap-creation-error (error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Could not create the xkb context"))))

(define-condition invalid-keysym-name (warning)
  ((name :initarg :name :reader invalid-keymap-name-name))
  (:report (lambda (condition stream)
	     (format stream "Keymap name ~S is invalid"
		     (invalid-keymap-name-name condition)))))

(define-condition invalid-keysym-code (warning)
  ((keysym :initarg :keysym :reader invalid-keysym-code-code))
  (:report (lambda (condition stream)
	     (format stream "Keysym code ~S is invalid"
		     (invalid-keysym-code-code condition)))))

;; a couple of opaque structs:
(defcstruct keymap)

(defcstruct context)

(defcstruct state)

(declaim (inline xkb-keysym-get-name))
(defcfun "xkb_keysym_get_name" :int
  (keysym keysym)
  (buffer (:pointer :char))
  (size :size))

(defun keysym-get-name (keysym)
  (declare (type (unsigned-byte 32) keysym))
  (cffi:with-foreign-pointer-as-string ((buffer size) 15)
    (xkb-keysym-get-name keysym buffer size)))

(define-compiler-macro keysym-get-name (&whole whole keysym)
  (if (constantp keysym)
      (let ((name (keysym-get-name keysym)))
	(if name
	    name
	    (progn
	      (warn 'invalid-keysym-code :keysym keysym)
	      name)))
      whole))

(declaim (inline xkb-keysym-from-name))
(defcfun "xkb_keysym_from_name" keysym
  (name :string)
  (flags keysym-flags))

(defun keysym-from-name (name &optional (flags :no-flags))
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (xkb-keysym-from-name name flags)))
    (declare (type (unsigned-byte 32) code))
    (if (= code 0)
	nil
	code)))

(define-compiler-macro keysym-from-name (&whole whole name &optional (flags :no-flags))
  (if (and (constantp name)  (constantp flags))
      (let ((keysym (keysym-from-name name flags)))
	(if keysym
	    keysym
	    (progn
	      (warn 'invalid-keysym-name :name name)
	      keysym)))
	whole))

(defcfun ("xkb_keysym_to_utf8" keysym-to-utf8) :int
  (keysym keysym)
  (buffer (:pointer :char))
  (size :size))

(declaim (inline keycode-is-legal-ext-p keycod-is-legal-x11-p))
(defun keycode-is-legal-ext-p (key)
  (<= key +keycode-max+))

(defun keycode-is-legal-x11-p (key)
  (and (>= key 8) (<= key 255)))

(defcfun "xkb_context_new" (:pointer (:struct context))
  (context context-flags))

(defcfun "xkb_keymap_new_from_names" (:pointer (:struct keymap))
  (context (:pointer (:struct context)))
  (names (:pointer (:struct rule-names)))
  (flags compile-flags))

(defcfun ("xkb_keymap_unref" keymap-unref) :void
  (keymap (:pointer (:struct keymap))))

(defcfun ("xkb_context_unref" context-unref) :void
  (context (:pointer (:struct context))))

(defcfun ("xkb_state_key_get_one_sym" state-key-get-one-sym) keysym
  (state (:pointer (:struct state)))
  (key keycode))

(defcfun ("xkb_state_key_get_syms" state-key-get-syms) :int
  (state (:pointer (:struct state)))
  (key keycode)
  (syms_out (:pointer (:pointer keysym))))

(defcfun ("xkb_keymap_key_get_name" keymap-key-get-name) :string
  (keymap (:pointer (:struct keymap)))
  (key keycode))

(defun new-context (context-flags)
  (let ((context (xkb-context-new context-flags)))
    (when (cffi:null-pointer-p context)
      (error 'context-creation-error))
    context))

(defun new-keymap-from-names (context names flags)
  (let ((keymap (xkb-keymap-new-from-names context names flags)))
    (when (cffi:null-pointer-p keymap)
      (error 'keymap-creation-error))
    keymap))

(defmacro with-keymap-from-names ((keymap-name (context rules flags)) &body body)
  `(let ((,keymap-name (new-keymap-from-names ,context ,rules ,flags)))
     (unwind-protect
	  (progn ,@body)
       (keymap-unref ,keymap-name))))

(defmacro with-xkb-context ((context-name (flags)) &body body)
  `(let ((,context-name (new-context ,flags)))
     (unwind-protect
	  (progn ,@body)
       (context-unref ,context-name))))

(defmacro with-xkb-rule-names ((rule-name (&key (rules "")
						(model "")
						(layout "")
						(variant "")
						(options "")))
			       &body body)
  "Bind RULE-NAME to a struct xkb_rule_names object within
the context and initalize its fields using the supplied values."
  (let ((rules-ptr (gensym "rules-ptr"))
	(model-ptr (gensym "model-ptr"))
	(layout-ptr (gensym "layout-ptr"))
	(variant-ptr (gensym "variant-ptr"))
	(options-ptr (gensym "options")))
    `(cffi:with-foreign-object (,rule-name '(:struct rule-names))
       (cffi:with-foreign-strings ((,rules-ptr ,rules)
				   (,model-ptr ,model)
				   (,layout-ptr ,layout)
				   (,variant-ptr ,variant)
				   (,options-ptr ,options))
	 (setf (cffi:foreign-slot-value ,rule-name '(:struct rule-names) :rules) ,rules-ptr
	       (cffi:foreign-slot-value ,rule-name '(:struct rule-names) :model) ,model-ptr
	       (cffi:foreign-slot-value ,rule-name '(:struct rule-names) :layout) ,layout-ptr
	       (cffi:foreign-slot-value ,rule-name '(:struct rule-names) :variant) ,variant-ptr
	       (cffi:foreign-slot-value ,rule-name '(:struct rule-names) :options) ,options-ptr)
	 ,@body))))


(defcfun ("xkb_keymap_num_mods" keymap-num-mods) mod-index
  (keymap (:pointer (:struct keymap))))

(defcfun ("xkb_keymap_mod_get_name" keymap-mod-get-name) :string
  (keymap (:pointer (:struct keymap)))
  (index mod-index))

(defcfun ("xkb_keymap_mod_get_index" keymap-mod-get-index) mod-index
  (keymap (:pointer (:struct keymap)))
  (name :string))
