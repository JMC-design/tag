# tag
generic tags

(defgeneric tag          (object tag)
(:documentation "Adds '(:TAG . VALUE) to list of tags in OBJECT."))

(defgeneric get-tag      (object tag) 
(:documentation "Gets :TAG from OBJECT."))

(defgeneric get-all-tags (object)
(:documentation "Returns all TAGS for given OBJECT as alist."))
