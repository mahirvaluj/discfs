(defpackage :discfs
  (:use :cl :lispcord))
(in-package :discfs)

;; This is to load the token
(load #p"secret.lisp")
(if *token*
    (format t "*token* is ~a~%" *token*)
    (error "discfs:*token* not defined in src/secret.lisp"))

(defbot *discfs* *token*)

(defvar *mntc*)
(defvar *dbg-chnl*)

(add-event-handler
 :on-ready
 #'(lambda (ready)
     (format t "User ~a~%Session ~a~%is ready~%"
             (lc:name (lc:user ready))
             (lc:session-id ready))))

;; making sure bot is alive
(add-event-handler
 :on-message-create
 (lambda (msg) (format t "message ID: ~a~%" (lc:id msg)) (if (string= (lc:content msg) "discfs:ping") (reply msg "pong!"))))

;; In all the following: HASH is a sha-256 hash, ID is a 64-bit uint
;; represented as hex

;; format of jump tables: 64 lines each containing a 16-char long hex
;; snowflake referring to the next jump table down the line.

;; to proceed through the jump table, take the sha-256 hash, grab the
;; lowest byte modulo 64 [bottom 6 bits] and look it up in the table,
;; then take the second lowest byte modulo 64 [bottom 6 bits] and look
;; it up in the second table, and then go to that message and search
;; linearly through a message chain until you find the hash, and then
;; store it there. If the message is in conflict, return error.

;; format of message chains: as many lines as can fit containing:
;; {HASH} {ID}
;; with a final message containing
;; NEXT {ID}
;; pointing to the next message in the chain

;; format of files: base64!
;; {BLOB}
;; NEXT {id}
;; the NEXT line is optional 

(defun init-channel (channel-id)
  "this creates the jump tables. Takes forever. sorry."
  (let ((c (lispcord.http:from-id channel-id :channel)))
    (unless (= 0 (length (lispcord.http:get-pinned c)))
      (error "Channel already has pinned messages, did you mean to mount the channel?"))
    (let ((v (make-array 64 :element-type 'integer))
          (vouter (make-array 64 :element-type 'integer)))
      (dotimes (i 64)
        (dotimes (j 64)
          (setf (aref v j) (lc:id (lispcord.http:create "placeholder" c))))
        (setf (aref vouter i) (lc:id (lispcord.http:create (with-output-to-string (s)
                                                             (loop for id across v do
                                                                  (format s "~x~%" id)))
                                                           c))))
      (lispcord.http:pin (lispcord.http:create
                          (with-output-to-string (s)
                            (loop for id across v do
                                 (format s "~x~%" id)))
                          c)))))

(defun mount-channel (channel-id)
  (setf *mntc* (lispcord.http:from-id channel-id :channel))
  (assert (not (= 0 (length (lispcord.http:get-pinned *mntc*))))))

(defun put (stream)
  "upload file into filesystem, and then return the hash at which this
  is stored, or NIL on error")

(defun del (hash)
  "delete file with given hash from filesystem, returning T on delete,
  NIL on no delete")

(defun get (hash)
  "get binary file from filesystem. returns '(unsigned-byte 8) stream")

;; (defun reg (id path)
;;   "this will add the hash to the filesystem in the given message. if
;;   the message is too full, go to NEXT message"
;;   (let ((msg (lispcord.http:from-id id :message)))
;;     ) )

(defun start ()
  (connect *discfs*))
