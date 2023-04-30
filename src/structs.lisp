;;;; polymorph.parametric-types.lisp

(in-package #:polymorph.parametric-types)


(defmacro def (name (&rest traits) &body slots)
  "Defines a structure with polymorphic constructor and accessors. :mut is for declaring slots
mutable. Default values are filled according to types. Doesn't have syntax for inheritance.
You can provide :eq and :copy as indicators that there should also be = and copy defined.
Example of usage:
  (def user (:eq :copy)
   (name simple-string)
   (age (integer 0 200)))"
  (let ((typed-slots
          (loop :for slot :in slots
                :collect (if (listp slot)
                             (destructuring-bind
                                     (sname &optional (stype t) (sform (default stype)))
                                     slot
                                   `(,sname ,stype ,sform))
                             `(,slot t nil)))))
    `(progn
       (defstruct ,name
         ,@(loop :for (sname stype sform) :in typed-slots
                 :collect `(,sname ,sform
                                   :type ,stype)))
       ;,(unless (fboundp name)
       ;   (when *with-constructor*
       ;     `(define-polymorphic-function ,name (&key ,@(loop :for (sname) :in typed-slots
       ;                                                       :collect sname)))
       ;,(when *with-constructor*
       ;   `(defpolymorph (,name :inline t) (&key ,@(loop :for (sname stype sform) :in typed-slots
       ;                                                  :collect (list (list sname stype) sform))
       ;        (values ,name &optional)
       ;      (,(alexandria:symbolicate 'MAKE- name)
       ;       ,@(loop :for (sname) :in typed-slots
       ;               :appending (list (intern (string sname) "KEYWORD") sname)))
       ,@(loop :for (sname stype _) :in typed-slots
               :unless (fboundp sname)
                 :collect `(define-polymorphic-function ,sname (object) :overwrite t)
               :collect `(defpolymorph (,sname :inline t) ((,name ,name)) (values ,stype &optional)
                           (,(alexandria:symbolicate name '- sname)
                            ,name))
               :unless (fboundp `(cl:setf ,sname))
                 :collect `(define-polymorphic-function (cl:setf ,sname) (new object) :overwrite t)
                 :and :collect `(defpolymorph ((cl:setf ,sname) :inline t) ((new ,stype) (,name ,name)) (values ,stype &optional)
                                  (cl:setf (,(alexandria:symbolicate name '- sname)
                                            ,name)
                                           new)))
       ,(when (member :eq traits)
          `(defpolymorph (= :inline t) ((first ,name) (second ,name)) boolean
             (and ,@(loop :for (sname) :in typed-slots
                          :collect `(= (,sname first) (,sname second))))))
       ,(when (member :copy traits)
          `(progn
             (defpolymorph (deep-copy :inline t) ((object ,name)) ,name
               (,(intern (format nil "MAKE-~s" name))
                ,@(loop :for (sname) :in typed-slots
                        :appending `(,(intern (string sname) "KEYWORD") (deep-copy (,sname object))))))
             (defpolymorph (shallow-copy :inline t) ((object ,name)) ,name
               (,(intern (format nil "MAKE-~s" name))
                ,@(loop :for (sname) :in typed-slots
                        :appending `(,(intern (string sname) "KEYWORD") (,sname object)))))))
       ',name)))
