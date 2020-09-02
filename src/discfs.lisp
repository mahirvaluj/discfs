(defpackage :discfs
  (:use :cl :lispcord))

(in-package :discfs)



(defvar *token* (sb-unix::posix-getenv "DISCFS-TOKEN"))

(defbot *token*)


(defun connect (&optional token)
  (if *token*
      (progn
        
        (connect *token*))
      
      (error "*token* not defined (or, DISCFS-TOKEN env var not in env)")))

