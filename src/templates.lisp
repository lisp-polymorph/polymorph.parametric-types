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
(defmacro template (template-args traits &body template-forms)

  `(progn
     ,@(loop :for template :in template-forms
            :collect (gen-defintion template template-args))))


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defun gen-defintion (template-form template-args)
   (case (first template-form)
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
      (let ((constructor-form `(constructor-name
                                ,@(loop :for (sname) :in slots
                                      :appending (list (intern (string sname) "KEYWORD") sname)))))
            
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
                        `(,slot-name :initarg ,(intern (string slot-name) "KEYWORD")
                                     :accessor ,slot-name))))

             (defmacro ,(alexandria:symbolicate 'define- name) ,template-args
               (let* ((,name (encode-name (cons ',name (list ,@template-args))))) ;;FIXME hacky solution
                    ;  (constructor-name (alexandria:symbolicate 'MAKE- native-name)))
                 (unless (equal (get ,name 'template-struct)
                                (cons ',name (list ,@template-args)))
                   (prog1
                       `(eval-when (:compile-toplevel
                                    :load-toplevel
                                    :execute)
                          ,,(%emulate-backquote template-form (cons name template-args)))
                          ;(defmethod default-form ((type (eql ',native-name)))
                          ;  ,,(%emulate-backquote constructor-form-with-defaults '(constructor-name)))
                             ;;especially here
                     (setf (get ,name 'template-struct) (list ',name ,@template-args))))))
                           ;(get ,',name 'template-ref)
                           ;(make-instance ',(alexandria:symbolicate 'c- ,name)
                           ;               ,@',(loop :for (name type val) :in temp
                           ;                         :for slot-name := (alexandria:symbolicate '% name)
                           ;                         :collect (intern (string slot-name) "KEYWORD")
                            ;                        :collect type))))))

             (defun ,(alexandria:symbolicate 'ensure- name) ,template-args
               (eval (list ',(alexandria:symbolicate 'define- name)
                           ,@template-args)))

             (deftype ,name ,template-args
               (let* ((native-name (encode-name (cons ',name (list ,@template-args)))))
                 (unless (equal (get native-name 'template-struct)
                                (cons ',name (list ,@template-args)))
                   (,(alexandria:symbolicate 'ensure- name) ,@template-args))
                 native-name))

             (defpolymorph (,name :inline t)
                 (,@(mapcar (lambda (x) (list x '(or symbol cons))) template-args)
                  &key ,@(loop :for (sname stype sform) :in slots
                               :collect (list sname (if (member stype template-args)
                                                        t
                                                        stype))))
                                                 ;`(default ,stype)))))
                 (values t &optional)           ;; TODO at this point struct is not defined yet, so cannot declare return type
               (let* ((native-name (encode-name (cons ',name (list ,@template-args)))))
                 (unless (equal (get native-name 'template-struct)
                                (cons ',name (list ,@template-args)))
                   (,(alexandria:symbolicate 'ensure- name) ,@template-args))
                 (funcall (alexandria:symbolicate 'make- native-name)
                  ,@(loop :for (sname) :in slots
                          :appending (list (intern (string sname) "KEYWORD") sname)))))

             (defpolymorph-compiler-macro ,name
                 (,@(loop :repeat (length template-args) :collect '(or symbol cons))
                  &key
                  ,@(loop :for (sname stype _sform) :in slots
                          :collect `(,(intern (string sname) "KEYWORD")
                                     ,(if (member stype template-args)
                                         t
                                         stype))))
                 (&whole form ,@template-args
                         &key
                         ,@(loop :for (sname stype sform) :in slots
                                 :collect sname)
                                 ;(list sname
                                 ;      (or sform
                                 ;          `(default ,stype))
                         &environment env)
               (declare (ignorable form))
               (assert (and ,@(loop :for tname :in template-args
                                    :collect `(constantp ,tname))))
               (let* ,(loop :for tname :in template-args
                            :collect `(,tname (eval ,tname)))
                 ,@(loop :for (name type val) :in temp
                        :collect `(assert (subtypep (%form-type ,name env) ,type env)
                                   (,name) 'type-error :expected-type ,type :datum ,name))
                 (let* ((native-name (encode-name (cons ',name (list ,@template-args))))
                        (constructor-name (alexandria:symbolicate 'MAKE- native-name)))
                   (unless (equal (get native-name 'template-struct)
                                  (cons ',name (list ,@template-args)))
                     (,(alexandria:symbolicate 'ensure- name) ,@template-args))
                   `(the ,native-name
                        ,,(%emulate-backquote constructor-form (cons 'constructor-name
                                                                     (loop :for (sname stype sform) :in slots
                                                                         :collect sname
                                                                         :collect (intern (string sname) "KEYWORD")))))))))))))



  (defun get-template-arg-names (input template-args)
    (let ((names))
      (labels ((rec (list)
                (map nil (lambda (x)
                           (cond ((symbolp x) nil)
                                 ((and (listp x) (= 2 (length x))
                                       (symbolp (first x))
                                       (some (lambda (y) (find-tree y (second x))) template-args))
                                  (push (first x) names))
                                 ((listp x) (rec x))))
                        list)))
        (rec input)
        (reverse names))))



  (defun validate-template (arg-map template-map template-args)
    (let ((mapping (make-hash-table :test #'equal)))
      (loop :for (var-name . type) :in arg-map
            :for (_var-name . temp-type) :in template-map
            :do (assert (eql var-name _var-name))
                (let ((match-list (template-match temp-type type template-args)))
                  (loop :for (temp . actual-type) :in match-list
                        :do (multiple-value-bind (type-val ok) (gethash temp mapping)
                              (if ok
                                  (assert (alexandria:type= type-val actual-type)) ;;FIXME very poor way of checking
                                  (setf (gethash temp mapping) actual-type))))))
      mapping))






  (defun gen-polymorph-template-definition (template-form template-args)
    (declare (optimize (debug 3) (safety 3)))
    (destructuring-bind (_def name args return-type &body body) template-form
      (declare (ignorable _def body))
      (let* ((truename (if (listp name) (first name) name))
             (temp-names (get-template-arg-names args template-args))
             (templated-args (get-pf-template-arg-map args temp-names)))
        (multiple-value-bind (type-list arg-list) (parse-pf-lambda-list args)
          (let ((arg-names (mapcar (lambda (x) (if (symbolp x) x (first x))) arg-list)))
            `(eval-when (:compile-toplevel
                         :load-toplevel
                         :execute)
               (defmacro ,(alexandria:symbolicate 'define-pf- truename) ,template-args
                 (unless (find (list ,@template-args) (get ',name 'template-pf) ;;FIXME put this in utils
                               :test (lambda (l1 l2) (every (lambda (t1 t2) (alexandria:type= t1 t2)) l1 l2))
                               :key #'rest)
                   (let ((name ',name))
                     
                     (prog1
                         `(eval-when (:compile-toplevel
                                      :load-toplevel
                                      :execute)
                            ,,(%emulate-backquote template-form template-args))
                            ;(defpolymorph-compiler-macro ,',truename ,,(%emulate-backquote type-list template-args)
                            ;    ,',(append (mapcar (lambda (x)
                            ;                        (cond ((member x '(&optional &key &aux &rest &allow-other-keys))
                            ;                               x
                            ;                              ((symbolp x) (alexandria:symbolicate x '-form))
                            ;                              ((listp x) (list* (alexandria:symbolicate (first x) '-form)
                            ;                                                (rest x)
                            ;                      arg-list
                            ;               (list '&environment 'env)
                            ;   (let ,',(loop :for arg-name :in arg-names
                            ;                 :collect `(,arg-name (gensym ,(string arg-name)))
                            ;       ,,(%emulate-backquote
                            ;          (%emulate-backquote
                            ;           `(the ,return-type
                            ;             (let ,(loop :for arg-name :in arg-names
                            ;                         :collect `(,arg-name ,(alexandria:symbolicate arg-name '-form))
                            ;               ,@(loop :for arg-name :in arg-names
                            ;                       :collect `(declare (type (%form-type ,(alexandria:symbolicate arg-name '-form) env) ,arg-name))
                            ;               ,@body
                            ;           (append arg-names (mapcar (lambda (x) (alexandria:symbolicate x '-form)) arg-names)
                            ;                   (mapcar (lambda (x) `(%form-type ,(alexandria:symbolicate x '-form) env)) arg-names)
                            ;          template-args))
                              
                       (push (list ',name ,@template-args)
                             (get name 'template-pf))))))
               (defun ,(alexandria:symbolicate 'ensure-pf- truename) ,template-args
                 (eval (list ',(alexandria:symbolicate 'define-pf- truename)
                               ,@template-args)))
               (defpolymorph (,truename :inline t)
                 ,(replace-template-types args template-args)
                 ,(first (replace-template-types (list return-type) template-args))
                 (let ,(loop :for temp-name :in temp-names
                             :collect `(,(alexandria:symbolicate 'type- temp-name)
                                        (%type-of ,temp-name)))
                   (let* ((actual-types (list ,@(loop :for temp-name :in temp-names
                                                      :collect `(cons ',temp-name
                                                                 ,(alexandria:symbolicate 'type- temp-name)))))
                          (mapping (validate-template actual-types ',templated-args ',template-args)))
                     (apply #',(alexandria:symbolicate 'ensure-pf- truename) (mapcar (lambda (name) (gethash name mapping)) ',template-args))
                     ,@body)))
                     ;(,truename ,@(%get-pf-arg-names args))))))))))) ;; !FIXME! problem with code walking
               ,(multiple-value-bind (type-list arg-list) (parse-pf-lambda-list (replace-template-types args template-args))
                  `(defpolymorph-compiler-macro ,truename ,type-list
                       ,(append arg-list (list '&environment 'env))
                     (let ,(loop :for temp-name :in temp-names
                                 :collect `(,(alexandria:symbolicate 'type- temp-name)
                                            (lenient-form-type ,temp-name env)))
                       (let* ((actual-types (list ,@(loop :for temp-name :in temp-names
                                                          :collect `(cons ',temp-name
                                                                     ,(alexandria:symbolicate 'type- temp-name)))))
                              (mapping (validate-template actual-types ',templated-args ',template-args)))
                        ; (loop for k being the hash-keys in mapping using (hash-value v)
                         ;      do (format t "~a=>~a~%" k v)))
                         (apply #',(alexandria:symbolicate 'ensure-pf- truename) (mapcar (lambda (name) (gethash name mapping)) ',template-args))
                         ;`(,',truename ,@',(%get-pf-arg-names args)))))))))))))
                         ,(%emulate-backquote (list* 'funcall `#',truename (%get-pf-arg-names args))
                                              (%get-pf-arg-names args)))))))))))))

;; to be continued
(template
 (t1 t2)
 ()
 (def pair ()
   (fst t1)
   (snd t2)))


(defun tester (a b)
  (declare (optimize (speed 3))
           (fixnum a)
           (string b))
  (let ((x (pair '(pair fixnum string) '(pair fixnum string)
                 :fst (pair 'fixnum 'string :fst a :snd b)
                 :snd (pair 'fixnum 'string :fst a :snd b))))
    (declare (type (pair (pair fixnum string) (pair fixnum string)) x))
    (cl:+ (fst (fst x)) (fst (fst x)))))
#||
(template (t1)
    ()
  (defpolymorph foo ((a t1) (b t1)) (values t1 &optional)
    (cl:+ a b)))

(defun test (a b)
  (declare (optimize speed)
           (type fixnum a b))
  (foo a b))

(defun test2 (a b)
  (declare (optimize speed)
           (type single-float a b))
  (foo a b))


(template (t1)
    ()
  (defpolymorph at ((v (vector t1)) (i ind)) (values t1 &optional)
    (aref v i)))


(defun test3 (a)
  (declare (optimize speed)
           (type (vector string) a))
  (at a 10))
||#
(template (t1)
    ()
  (defpolymorph at ((v (vector t1)) (i ind)) (values t1 &optional)
    (aref v i)))


(defun test3 (a)
  (declare (optimize speed)
           (type (vector string) a))
  (at a 10))
