# tag
generic tags

(defgeneric add          (object tag)
(:documentation "Adds '(:TAG . VALUE) to list of tags in OBJECT."))

(defgeneric get          (object tag) 
(:documentation "Gets :TAG from OBJECT."))

(defgeneric get-all      (object)
(:documentation "Returns all TAGS for given OBJECT as alist."))

(defgeneric remove       (object tag)
(:documentation "Removes tag from object"))
