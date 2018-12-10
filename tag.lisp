(defpackage tag
  (:use :cl)
  (:export #:tag
	   #:get-tag
	   #:get-all-tags))
(in-package :tag)

(defgeneric tag          (object tag) (:documentation "Adds '(:TAG . VALUE) to list of tags in OBJECT."))
(defgeneric get-tag      (object tag) (:documentation "Gets :TAG from OBJECT."))
(defgeneric get-all-tags (object)     (:documentation "Returns all TAGS for given OBJECT as alist."))

;;;examples

;;symbol tags
(defmethod tag ((sym symbol) (tag cons))
  "Tags a symbol given a cons."
  (setf (get sym (car tag)) (cdr tag) ))
(defmethod get-tag ((sym symbol) tag)
  "gets a tag from a symbol"
  (get sym tag))
(defmethod get-all-tags ((sym symbol))
  (plist->alist (symbol-plist sym)))

;;xlib:windows
#+clx(progn
(defmethod tag ((win xlib:window) tag)
  (setf (getf (xlib:window-plist win) (car tag)) (cdr tag)))
(defmethod get-tag ((win xlib:window) tag)
  (getf (xlib:window-plist win) tag))
(defmethod get-all-tags ((win xlib:window))
  (plist->alist (xlib:window-plist win)))
)

;; utility
(defun plist->alist (lst)
  (loop :for tag := (pop lst)
     :for value := (pop lst)
     :while (and tag value)
     :collect (cons tag value)))
