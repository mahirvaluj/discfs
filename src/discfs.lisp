(defpackage :discfs
  (:use :cl :lispcord :ironclad :split-sequence)
  (:shadow :get))
(in-package :discfs)

;; This is to load the token
(load #p"secret.lisp")
(if (boundp '*token*)
    (format t "*token* is ~a~%" *token*)
    (error "discfs:*token* not defined in src/secret.lisp"))

(defbot *discfs* *token*)

(defvar *mntc*)
(defvar *jmp*)
(defvar *dbg-chnl*)

(add-event-handler
 :on-ready
 #'(lambda (ready)
     (format t "User ~a~%Session ~a~%is ready~%"
             (lc:name (lc:user ready))
             (lc:session-id ready))))

(defun bytes-to-uint64 (bytes)
  (let ((n 0))
    (do ((i 7 (- i 1))
         (j 0 (+ j 1)))
        ((= j 8))
      (setf n (+ n (ash (aref bytes j) (* 8 i)))))
    n))

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
          (format t "i ~d j ~d~%" i j)
          (setf (aref v j) (lc:id (lispcord.http:create "placeholder" c))))
        (setf (aref vouter i) (lc:id (lispcord.http:create (with-output-to-string (s)
                                                             (loop for id across v do
                                                                  (format s "~x~%" id)))
                                                           c))))
      (lispcord.http:pin (lispcord.http:create
                          (with-output-to-string (s)
                            (loop for id across vouter do
                                 (format s "~x~%" id)))
                          c)))))

(defun mount-channel (channel-id)
  (setf *mntc* (lispcord.http:from-id channel-id :channel))
  (let ((pinned (lispcord.http:get-pinned *mntc*)))
    (assert (not (= 0 (length pinned))))
    (setf *jmp* (aref pinned 0))))

(defun put (stream)
  "upload file into filesystem, and then return the hash at which this
  is stored, or NIL on error"
  (let ((arr (make-array 2048 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (off0) (off1) (jmpsnow) (filesnow))
    (loop while (listen stream)
       do (vector-push-extend (read-byte stream) arr))
    (let ((sha256sum (ironclad:digest-sequence :sha256 arr)))
      (setf off0 (mod (aref sha256sum (- (length sha256sum) 1)) 64))
      (setf off1 (mod (aref sha256sum (- (length sha256sum) 2)) 64))
      ;(format t "0: ~d  1: ~d~%" off0 off1)
      (setf jmpsnow (bytes-to-uint64
                       (ironclad:hex-string-to-byte-array
                        (format nil "0~a" (nth off0 (split-sequence:split-sequence #\Newline (lc:content *jmp*)))))))
      (setf filesnow (bytes-to-uint64
                    (ironclad:hex-string-to-byte-array
                     (format nil "0~a" (nth off1 (split-sequence:split-sequence #\Newline (lc:content (lispcord.http:from-id jmpsnow *mntc*)))))))))
    (format t "j: ~x  f: ~x" jmpsnow filesnow)))

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
