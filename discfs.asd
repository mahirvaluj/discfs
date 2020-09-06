(defsystem "discfs"
    :depends-on ("lispcord")
    :author "seanptmaher@gmail.com"
    :license "MIT"
    :components
    ((:module src
              :sertial t
              :components
              ((:file "discfs")
               (:file "secret")))))
