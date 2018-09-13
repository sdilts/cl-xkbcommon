(in-package :xkb)

(export '(with-keymap-from-names
	  with-xkb-context))

(defmacro with-keymap-from-names ((keymap-name (context rules flags)) &body body)
  `(let ((,keymap-name (new-keymap-from-names ,context ,rules ,flags)))
     ,@body
     (keymap-unref ,keymap-name)))

(defmacro with-xkb-context ((context-name (flags)) &body body)
  `(let ((,context-name (new-context ,flags)))
     ,@body
     (context-unref ,context-name)))
