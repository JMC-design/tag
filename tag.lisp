(defpackage tag
  (:use :cl)
  (:shadow GET REMOVE)
  (:export #:add
	   #:get
	   #:remove
	   #:get-all))
(in-package :tag)

(defgeneric add          (object tag) (:documentation "Adds '(:TAG . VALUE) to list of tags in OBJECT."))
(defgeneric get          (object tag) (:documentation "Gets :TAG from OBJECT."))
(defgeneric remove       (object tag) (:documentation "Removes :TAG from OBJECT."))
(defgeneric get-all      (object)     (:documentation "Returns all TAGS for given OBJECT as alist."))

;;;examples
;; utility
(declaim (inline plist->alist))
(defun plist->alist (list)
  (loop :for tag := (pop list)
	:for value := (pop list)
	:while (and tag value)
	:collect (cons tag value)))

;;symbol tags
(defmethod add     ((sym symbol) (tag cons)) (setf (get sym (car tag)) (cdr tag) ))
(defmethod get     ((sym symbol) tag)        (get sym tag))
(defmethod remove  ((sym symbol) tag)        (remprop sym tag))
(defmethod get-all ((sym symbol))            (plist->alist (symbol-plist sym)))

;;; The obvious
(defmethod add     ((list list) (tag cons))  (append list (list (car tag) (cdr tag))))
(defmethod get     ((list list) tag)         (getf list tag))
(defmethod remove  ((list list) tag)         (remf list tag))
(defmethod get-all ((list list))             list)

;; have function to go through source and replace all instances of a generic function with an inlined function
;;xlib:windows
#+clx(progn
(defmethod add     ((win xlib:window) tag) (setf (getf (xlib:window-plist win) (car tag)) (cdr tag)))
(defmethod get     ((win xlib:window) tag) (getf (xlib:window-plist win) tag))
(defmethod remove  ((win xlib:window) tag) (remf (xlib:window-plist win) tag))
(defmethod get-all ((win xlib:window))     (plist->alist (xlib:window-plist win))))
