(in-package #:cl-xkb/common)

(defcfun ("xkb_keysym_get_name" keysym-get-name) :int
  (keysym keysym)
  (buffer (:pointer :char))
  (size size-t))

(defcfun ("xkb_keysym_from_name" keysum-from-name) keysym
  (name :string)
  (flags keysym-flags))

(defcfun ("xkb_keysym_to_utf8" keysym-to-utf8) :int
  (keysym keysym)
  (buffer (:pointer :char))
  (size size-t))

(declaim (inline keycode-is-legal-ext-p keycod-is-legal-x11-p))
(defun keycode-is-legal-ext-p (key)
  (<= key +keycode-max+))

(defun keycod-is-legal-x11-p (key)
  (and (>= key 8) (<= key 255)))
