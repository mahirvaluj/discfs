(defpackage :discfs
  (:use :cl :lispcord :ironclad :split-sequence :qbase64 :str)
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


;; making sure bot is alive
(add-event-handler
 :on-message-create
 (lambda (msg) (format t "message ID: ~a~%" (lc:id msg)) (if (string= (lc:content msg) "discfs:ping") (reply msg "pong!"))))

(defun bytes-to-uint64 (bytes)
  (let ((n 0))
    (do ((i 7 (- i 1))
         (j 0 (+ j 1)))
        ((= j 8))
      (setf n (+ n (ash (aref bytes j) (* 8 i)))))
    n))

(defun hex-to-snowflake (hx)
  "handles the fact that the snowflakes might need a padding 0 and converts hex string into snowflake integer"
  (bytes-to-uint64
   (ironclad:hex-string-to-byte-array
    (if (not (= (length hx) 16))
        (format nil "0~a" hx)
        hx))))

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
;; with a line in it someplace containing
;; NEXT {ID}
;; pointing to the next message in the chain. This doesn't have to be at the end.

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

(defun resolve-to-record (arr)
  "jumps through tables to get to final <placeholder> message which
  will point to actual file"
  (let ((off0) (off1) (jmpsnow))
    (let ((sha256sum (ironclad:digest-sequence :sha256 arr)))
      (setf off0 (mod (aref sha256sum (- (length sha256sum) 1)) 64))
      (setf off1 (mod (aref sha256sum (- (length sha256sum) 2)) 64))
      ;;(format t "0: ~d  1: ~d~%" off0 off1)
      (setf jmpsnow
            (hex-to-snowflake
             (nth off0
                  (split-sequence:split-sequence #\Newline (lc:content *jmp*)))))
      (lispcord.http:from-id (hex-to-snowflake
                              (nth off1
                                   (split-sequence:split-sequence #\Newline (lc:content (lispcord.http:from-id jmpsnow *mntc*)))))
                             *mntc*))))

;; will upload chunks of 1425 bytes (as this will allow for 
(defun upload-file (file-arr)
  "upload file as base64 blobs in channel, then return snowflake for
  head message or NIL on fail"
  (let ((b64 (qbase64:encode-bytes file-arr))
        (acc))
    (loop for i from 0 to (length b64) by 1900
       do (push (subseq b64 i (min (length b64) (+ i 1900))) acc))
    ;; because we're pushing the bits, acc is already ordereed from
    ;; the end to the start of the file! magic!
    (let ((psnow))
      (dolist (b64subst acc)
        (setf psnow (lc:id (lispcord.http:create (if (not (null psnow))
                                                     (format nil "~a~%NEXT ~x" b64subst psnow)
                                                     b64subst)
                                                 *mntc*)))
        (when (null psnow)
          (return-from upload-file nil)))
      psnow)))

(defun add-to-record (record file-arr)
  "uploads the file then adds it to the record"
  ;; refresh record (because previous edits of the record could have
  ;; caused it to get out of sync
  (setf record (lispcord.http:from-id (lc:id record) *mntc*))
  (let ((sha256hx (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 file-arr))))
    (when (not (null (search sha256hx (lc:content record))))
      ;; this means hash collision
      (return-from add-to-record nil))
    (let ((file-head (upload-file file-arr)))
      (when (null file-head)
        (error "File upload failed"))
      (cond ((string= (lc:content record) "placeholder")
             (lispcord.http:edit (format nil "~a ~a" sha256hx file-head) record))
            ((<= (+ 110 (length (lc:content record))) 2000)
             ;; we can safely append here. Hash is 64 long, plus 17
             ;; for ID of b64 file dump, plus 29 for hypothetical NEXT
             (lispcord.http:edit (concatenate 'string
                                              (str:replace-all (format nil "~%") "\\n" (lc:content record))
                                              (format nil "\\n~a ~a" sha256hx file-head))
                                 record))
            ((not (null (search "NEXT" (lc:content record))))
             ;; this means this record is full (previous check didn't
             ;; succeed) and there exists a NEXT, go to NEXT
             (let* ((next-index (search "NEXT" (lc:content record)))
                    (next-record (lispcord.http:from-id
                                  (hex-to-snowflake (subseq (lc:content record)
                                                            (+ 5 next-index)
                                                            (min (length (lc:content record))
                                                                 (let ((v (search (format nil "~%")
                                                                                  (lc:content record)
                                                                                  :start2 next-index)))
                                                                   (if (not (null v))
                                                                       v
                                                                       2100)))))
                              *mntc*)))
               (add-to-record next-record file-arr)))
            (t
             ;; this means search for NEXT failed and we need to create a new NEXT
             (let ((new-record (lispcord.http:create "placeholder" *mntc*)))
               (lispcord.http:edit (concatenate 'string
                                                (str:replace-all (format nil "~%") "\\n" (lc:content record))
                                                (format nil "\\nNEXT ~x" (lc:id new-record)))
                                   record)))))))

(defun put (stream)
  "upload file into filesystem, and then return the hash at which this
  is stored, or NIL on error"
  (let ((arr (make-array 2048 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop while (listen stream)
       do (vector-push-extend (read-byte stream) arr))
    (add-to-record (resolve-to-record arr) arr)))


(defun del (hash)
  "delete file with given hash from filesystem, returning T on delete,
  NIL on no delete"
  )

(defun get (hash)
  "get binary file from filesystem. returns '(unsigned-byte 8) stream")

;; (defun reg (id path)
;;   "this will add the hash to the filesystem in the given message. if
;;   the message is too full, go to NEXT message"
;;   (let ((msg (lispcord.http:from-id id :message)))
;;     ) )

(defun start ()
  (connect *discfs*))
