(defsystem "discfs"
  :depends-on ("lispcord" "ironclad" "split-sequence" "qbase64" "str")
  :author "seanptmaher@gmail.com"
  :license "MIT"
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "secret")
             (:file "discfs")))))
