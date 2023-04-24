;;;; polymorph.parametric-types.lisp

(in-package #:polymorph.parametric-types)



#||
(template (t1 t2 t3)
  (defpolymorph ((setf foo) :inline t) (a b c d)))


(template (t1 t2 t3)
  (def pair ()
    (first t1)
    (second (cons t2 t3))))

||#




(defmacro template (template-args traits &rest template-forms)

  `(progn
     ,@(loop :for template :in template-forms
            :collect (gen-defintion template template-args))))



(defun gen-defintion (template-form template-args)
  (case template-form
    (def (gen-struct-template-defintion template-form template-args))
    (defpolymorph (gen-polymorph-template-definition template-form template-args))
    (definstance (gen-instance-template-definition template-form template-args))
    (otherwise (error "Unknown template form"))))


(defun parse-slots (slots template-names)
  (let ((templated-slots)
        (normal-slots))
    (loop :for (name type val) :in slots
          :do (if (find-all-tree template-names type)
                  (push (list name type val) templated-slots)
                  (push (list name type val) normal-slots)))
    (values (reverse templated-slots) (reverse normal-slots))))

(defun gen-struct-template-defintion (template-form template-args) ;;TODO This might require typeexpand somewhere
  (destructuring-bind (_def name traits &rest slots) template-form ;;too confused now to figure out where
    (declare (ignorable _def traits))
    (multiple-value-bind (temp norm) (parse-slots slots template-args)
      (declare (ignorable norm))
      `(eval-when (:compile-toplevel
                   :load-toplevel
                   :execute)
         (defclass ,(alexandria:symbolicate 'c- name) (ctype:ctype)
           ,(loop :for (name type val) :in temp
                  :collect
                  (let ((slot-name
                          (alexandria:symbolicate '% name)))
                    `(,slot-name :initarg ,(intern (string name) "KEYWORD")
                                 :accessor ,slot-name))))

         (defmacro ,(alexandria:symbolicate 'define- name) ,template-args
           (let* ((native-name (encode-name (cons ',name (list ,@template-args)))))
             (unless (equal (get native-name 'template)
                            (cons ',name (list ,@template-args)))
               (setf (get native-name 'template) (cons ',name (list ,@template-args)))
               `(eval-when (:compile-toplevel
                            :load-toplevel
                            :execute)
                  ,',(%emulate-backquote template-form template-args)))))

         (defun ,(alexandria:symbolicate 'ensure- name) ,template-args
           (eval (list ',(alexandria:symbolicate 'define- name)
                       ,@template-args)))

         (deftype ,name ,template-args
           (let* ((native-name (encode-name (cons ',name (list ,@template-args)))))
             (unless (equal (get native-name 'template)
                            (cons ',name (list ,@template-args)))
               (,(alexandria:symbolicate 'ensure- name) ,@template-args)
               native-name)))

         (defpolymorph (,name :inline t) (,@(mapcar (lambda (x) (list x 'symbol)) template-args)))))))
;; to be continued
