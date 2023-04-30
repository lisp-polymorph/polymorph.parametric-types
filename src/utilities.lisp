;;;; polymorph.parametric-types.lisp

(in-package #:polymorph.parametric-types)


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)

  (defparameter *with-constructor* t)

  (defun encode-name (symbols)
    (labels ((rec (symbols)
               (alexandria:symbolicate
                '|(| (reduce (lambda (whole new)
                               (if (symbolp new)
                                   (alexandria:symbolicate whole '| | new)
                                   (alexandria:symbolicate whole '| | (rec new))))
                             symbols) '|)|)))
      (rec symbols)))


  (defun decode-name (name)
    (read-from-string (string name)))



  (defun find-tree (name tree)
    (labels ((rec (tree)
               (if (listp tree)
                   (map nil (lambda (x)
                              (if (listp x)
                                  (rec x)
                                  (when (eql x name)
                                    (return-from find-tree t))))
                        tree)
                   (eql tree name))))
      (rec tree)))

  (defun find-all-tree (names tree)
    (loop :for name :in names
          :thereis (find-tree name tree)))


  (defun find-interface (name)
    (or (get name 'interface) (error "No such interface")))



  (defun same-form-p (interface instance)
    (if (symbolp interface)
        t
        (and (listp instance)
             (= (length interface) (length instance))
             (loop :for x :in interface
                   :for y :in instance
                   :always (cond ((keywordp x)
                                  (eql x y))
                                 ((symbolp x)
                                  t)
                                 ((listp x)
                                  (and (listp y) (same-form-p x y))))))))


  (defun map-interface-args (interface instance ht)
    (if (symbolp interface)
        (setf (gethash interface ht) instance)
        (loop :for x :in interface
              :for y :in instance
              :do (unless (keywordp x)
                    (map-interface-args x y ht)))))

  (defun map-tree (tree function)
    (labels ((rec (tree)
               (mapcar (lambda (x)
                         (if (listp x)
                             (rec x)
                             (funcall function x)))
                       tree)))
      (rec tree)))

  (defun %emulate-backquote (tree args)
    (labels ((rec (tree)
               (if (listp tree)
                   `(list ,@(mapcar #'rec tree))
                   (if (member tree args :test #'equal)
                       tree
                       `',tree))))

      (rec tree)))

  (defun getalist (key alist)
    (rest (find key alist :key #'first)))


  (defun %map-args-to-pf-lambda-list (mapping pf-lambda-list) ;;FIXME needs rethinking
    (map-tree pf-lambda-list (lambda (x)
                               (or (gethash x mapping)
                                   x))))


  (defun %get-pf-arg-names (pf-arg-list)
    (let ((names))
      (labels ((rec (x)
                 (cond ((and (symbolp x) (not (member x '(&optional &key &rest &aux &allow-other-keys))))
                        (push x names))
                       ((listp x)
                        (rec (first x))))))
        (map nil #'rec pf-arg-list)
        (reverse names))))



  (defun %type-of (object)
    (let ((cl-runtime-type (type-of object)))
      (cond ((eql 'bit cl-runtime-type) (type-of 2))
            ((symbolp cl-runtime-type) cl-runtime-type)
            ((listp cl-runtime-type)
             (destructuring-bind (typename &rest args) cl-runtime-type
               (cond ((member typename '(simple-array vector array))
                      (list typename (first args)
                            (if (listp (second args))
                                (mapcar (lambda (x) (declare (ignore x)) 'cl:*) (second args))
                                (second args))))
                     ((member typename '(string simple-string simple-vector
                                         simple-base-string simple-bit-vector))
                      (list typename 'cl:*))
                     (t cl-runtime-type))))
            (t (error "Unrecognized type"))))) ;; unreachable?



  (defun lenient-form-type (form &optional env)
    (let ((strict-form-type (%form-type form env)))
      (cond ((constantp form)
             (%type-of (eval form)))
            (t strict-form-type))))


  (defun replace-template-types (list template-args)
    (labels ((rec (x)
               (cond ((and (symbolp x) (member x template-args))
                      t)
                     ((and (listp x) (find-if (lambda (y) (member y template-args)) x))
                      (mapcar (lambda (y) (if (member y template-args)
                                              'cl:*
                                              y))
                              x))
                     ((listp x) (mapcar #'rec x))
                     (t x))))
      (mapcar (lambda (x) (cond ((listp x)
                                 (mapcar #'rec x))
                                (t x)))
              list)))

  (defun get-pf-template-arg-map (lambda-list template-names)
    (let ((result))
      (labels ((rec (x)
                 (cond ((and (listp x) (= 2 (length x)) (symbolp (first x)) (member (first x) template-names))
                        (push (cons (first x) (second x)) result))
                       ((listp x) (mapcar #'rec x)))))
        (rec lambda-list)
        (reverse result))))


  (defun parse-pf-lambda-list (lambda-list)
    (let ((types) (args) (kind))
      (flet ((add-type (name type)
               (case kind
                 (&key (push (list (intern (string name) "KEYWORD") type) types))
                 (&rest nil)
                 (otherwise (push type types)))))
        (loop :for arg :in lambda-list
              :if (not (member arg '(&optional &key &rest &aux &allow-other-keys)))
                :do (cond ((symbolp arg)
                           (add-type arg t)
                           (push arg args))
                          ((listp arg)
                           (destructuring-bind (name-and-maybe-type &rest defaults) arg
                             (if (listp name-and-maybe-type)
                                 (progn
                                   (add-type (first name-and-maybe-type) (second name-and-maybe-type))
                                   (push (list* (first name-and-maybe-type) defaults) args))
                                 (progn
                                   (push name-and-maybe-type args)
                                   (add-type name-and-maybe-type (first defaults)))))))
              :else :do (push arg types)
                        (push arg args)
                        (setf kind arg))
       (values (reverse types) (reverse args))))))
