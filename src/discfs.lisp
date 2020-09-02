(defpackage :discfs
  (:use :cl :lispcord))

(in-package :discfs)
;; This is to load the token
(load #p"secret.lisp")
(if *token*
    (format t "*token* is ~a~%" *token*)
    (error "*token* not defined in src/secret.lisp"))

(defbot *discfs* *token*)

(add-event-handler
 :on-ready
 #'(lambda (ready)
     (format t "User ~a~%Session ~a~%is ready~%"
             (lc:name (lc:user ready))
             (lc:session-id ready))))

(add-event-handler
 :on-message-create
 (lambda (msg) (if (string= (lc:content msg) "ping!") (reply msg "pong!"))))


(defun start (channel-id)
  (connect *discfs*))
