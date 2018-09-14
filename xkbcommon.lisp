(in-package #:xkb)

(export '(;; functions
	  keysym-get-name
	  keysym-from-name
	  keysym-to-utf8
	  keycode-is-legal-ext-p
	  keycode-is-legal-x11-p
	  new-context
	  new-keymap-from-names
	  keymap-key-get-name
	  keymap-unref
	  context-unref
	  state-key-get-syms
	  state-key-get-one-sym
	  state-key-get-syms
	  ;; enums, structs, ect
	  mod-index
	  mod-mask
	  modifier-names
	  led-names
	  keycode
	  keysym
	  layout-index
	  layout-mask
	  level-index
	  led-index
	  invalid-masks
	  +keycode-max+
	  rule-names
	  keysym-flags
	  context-flags
	  compile-flags
	  state-component
	  state-match
	  consumed-mode
	  keymap
	  context
	  state
	  ;; conditions
	  context-creation-error
	  keymap-creation-error))

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


(defcfun ("xkb_keysym_get_name" keysym-get-name) :int
  (keysym keysym)
  (buffer (:pointer :char))
  (size size-t))

(defcfun ("xkb_keysym_from_name" keysym-from-name) keysym
  (name :string)
  (flags keysym-flags))

(defcfun ("xkb_keysym_to_utf8" keysym-to-utf8) :int
  (keysym keysym)
  (buffer (:pointer :char))
  (size size-t))

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
