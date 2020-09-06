(defsystem "discfs"
  :depends-on ("lispcord" "ironclad" "split-sequence" "qbase64" "str")
  :author "seanptmaher@gmail.com"
  :license "MIT"
  :components
  ((:module src
            :sertial t
            :components
            ((:file "discfs")
             (:file "secret")))))
