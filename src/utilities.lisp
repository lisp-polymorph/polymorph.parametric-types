;;;; polymorph.parametric-types.lisp

(in-package #:polymorph.parametric-types)


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)

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
                   (if (member tree args)
                       tree
                       `',tree))))

      (rec tree)))


  (defun %map-args-to-pf-lambda-list (mapping pf-lambda-list) ;;FIXME needs rethinking
    (map-tree pf-lambda-list (lambda (x)
                               (or (gethash x mapping)
                                   x)))))
