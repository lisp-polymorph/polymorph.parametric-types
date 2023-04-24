;;;; polymorph.parametric-types.asd

(asdf:defsystem #:polymorph.parametric-types
  :description "Parametric types for polymorph.stl"
  :author "Commander Thrashdin"
  :license  "MIT"
  :version "1.0"
  :serial t
  :depends-on (#:polymorphic-functions #:introspect-ctype)
  :components ((:module
                  "src"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "utilities")
                   (:file "structs")
                   (:file "interfaces")
                   (:file "templates")))))
