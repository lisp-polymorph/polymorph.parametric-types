;;;; polymorph.parametric-types.lisp

(in-package #:polymorph.parametric-types)

#||
(template t1
  ()
  (definstance name ((vector :item t1))))


(template t1
  ()
  (defpolymorph at ((v (vector t1)) (i ind)) (values t1 &optional)))
||#

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)


 (defclass pf-lambda-list ()
   ((required :type list
              :initarg :req
              :accessor req
              :initform nil)
    (optional :type list
              :initarg :opt
              :accessor opt
              :initform nil)
    (key :type list
         :initarg :key
         :accessor key
         :initform nil)
    (rst :type list
         :initarg :rst
         :accessor rst
         :initform nil)
    (returns :type list
             :initarg :returns
             :accessor returns
             :initform nil)))


 (defclass interface ()
   ((arguments :type list
               :initarg :args
               :accessor args
               :initform nil)
    (polymorphs :type list
                :initarg :pfs
                :accessor pfs)
    (instances :type list
               :initarg :inst
               :accessor inst
               :initform nil)))



 (defun %bad-parse-lambda-list (list returns)
   (make-instance 'pf-lambda-list
                  :req list
                  :returns (if (and (listp returns)
                                    (eql (first returns) 'cl:values))
                               (rest returns)
                               (list returns))))

 (defun validate-instance (name type-args)
    (let* ((interface (find-interface name))
           (interface-args (args interface))
           (mapping (make-hash-table)))
      (assert (= (length interface-args) (length type-args)))
      (loop :for type-arg :in type-args
            :for interface-arg :in interface-args
            :do (assert (same-form-p interface-arg type-arg))
                (map-interface-args interface-arg type-arg mapping))
      (loop :for (pfname . list) :in (pfs interface)
            :always (let* ((instantiated-type-list (%map-args-to-pf-lambda-list mapping list))
                           (instantiated-type-list (first (butlast instantiated-type-list)))) ;;FIXME dirty hack
                      (polymorphic-functions:find-polymorph pfname instantiated-type-list))))))

(defmacro definterface (name type-args &body body)
  (setf (get name 'interface)
        (make-instance 'interface
                       :args type-args
                       :pfs (loop :for (pfname lambda-list returns) :in body
                                  :collect (cons pfname
                                                 (list lambda-list returns))))))



    



(defmacro definstance (name type-args)
  (if (validate-instance name type-args)
      (progn (pushnew type-args (inst (get name 'interface)) :test #'equal)
             t)
      (error "Something is not implemented or types do not match"))) ;;FIXME makes sense of this error


;; testing area



(defpolymorph at ((b string) (a ind)) (values character &optional)
  (aref b a))

(defpolymorph at-safe ((b string) (a ind)) (values (or null character) boolean &optional)
  (if (< a (length b))
      (values (aref b a) t)
      (values nil nil)))


(definterface name ((a :item t1))
  (at (a ind) (values t1 &optional))
  (at-safe (a ind) (values (or null t1) boolean &optional)))

(definstance name ((string :item character)))
