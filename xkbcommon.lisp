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
  (cffi:with-foreign-pointer-as-string ((buffer size) 10)
    (xkb-keysym-get-name keysym buffer size)))

(declaim (inline xkb-keysym-from-name))
(defcfun "xkb_keysym_from_name" keysym
  (name :string)
  (flags keysym-flags))

(defun keysym-from-name (name flags)
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (xkb-keysym-from-name name flags)))
    (declare (type (unsigned-byte 32) code))
    (if (= code 0)
	nil
	code)))

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
     ,@body
     (keymap-unref ,keymap-name)))

(defmacro with-xkb-context ((context-name (flags)) &body body)
  `(let ((,context-name (new-context ,flags)))
     ,@body
     (context-unref ,context-name)))
