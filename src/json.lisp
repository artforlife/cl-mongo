(in-package :cl-mongo)

(defun get-elements-from-doc (document)
  (cl-mongo::elements document))

(defun document-type-p (object)
  (equal (type-of object) (type-of (cl-mongo:make-document))))

(defun list-type-p (object)
  (listp object))

(defun list-to-hash-form (object)
  (format t "Entered cl-mongo-list-to-hash")
  (let ((result-list nil))
    (dolist (element object)
      (format t "~% Processing: ~s" element)
    ;for now, we only handle docs as elements of array; not full JSON spec
      (when (document-type-p element)
	(push (document-to-hash-form element) result-list)))
    result-list))

(defun document-to-hash-form (document)
  (format t "~% Processing: ~s" document)
  (let ((result (get-elements-from-doc document)))
    (format t "~% The result contains ~s " result)
    (with-hash-table-iterator (hash-iterator result)
      (loop
	 (multiple-value-bind (entry-p key value)
	     (hash-iterator)
	   (format t "~% Processing:  ~s " key)
	   ;if the value under key is a document, call recrusively on it
	   (if entry-p
	       (cond ((document-type-p value)
		      (setf (gethash key result) (document-to-hash-form value)))
		     ((list-type-p value)
		      (setf (gethash key result) (list-to-hash-form value)))
		     (t (format t "~% ~s is neither a doc nor a list" key)))
	    (return))
	   )))
    result))

(defun cl-mongo-tester (hash-table)
  (with-hash-table-iterator (my-iterator hash-table)
    (loop
      (multiple-value-bind (entry-p key value)
          (my-iterator)
        (if entry-p
	    (cond ((document-type-p value) (format t "~% ~s is a document" key))
		  ((list-type-p value) (format t "~% ~s is a list" key))
		  (t (format t "~% ~s is neither a doc nor a list" key)))
	    (return))
	))))
