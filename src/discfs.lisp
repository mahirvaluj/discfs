(in-package :discfs)

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
  (start)
  (setf *mntc* (lispcord.http:from-id channel-id :channel))
  (let ((pinned (lispcord.http:get-pinned *mntc*)))
    (assert (not (= 0 (length pinned))))
    (setf *jmp* (aref pinned 0))))

(defmethod resolve-to-record (arr (tp (eql :file)))
  "jumps through tables to get to final <placeholder> message which
  will point to actual file"
  (resolve-to-record (ironclad:digest-sequence :sha256 arr) :hash))

(defmethod resolve-to-record (hash-str (tp (eql :hash)))
  (let ((off0) (off1) (jmpsnow))
    (setf off0 (mod (aref hash-str (- (length hash-str) 1)) 64))
    (setf off1 (mod (aref hash-str (- (length hash-str) 2)) 64))
    ;;(format t "0: ~d  1: ~d~%" off0 off1)
    (setf jmpsnow
          (hex-to-snowflake
           (nth off0
                (split-sequence:split-sequence #\Newline (lc:content *jmp*)))))
    (lispcord.http:from-id (hex-to-snowflake
                            (nth off1
                                 (split-sequence:split-sequence #\Newline (lc:content (lispcord.http:from-id jmpsnow *mntc*)))))
                           *mntc*)))

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

(defun next-record (recordio)
  (let ((record (lispcord.http:from-id (lc:id recordio) *mntc*)))
    (when (null (search "NEXT" (lc:content record)))
      (return-from next-record nil))
    (let ((next-index (search "NEXT" (lc:content record))))
      (lispcord.http:from-id
       (hex-to-snowflake (subseq (lc:content record)
                                 (+ 5 next-index)
                                 (min (length (lc:content record))
                                      (let ((v (search (format nil "~%")
                                                       (lc:content record)
                                                       :start2 next-index)))
                                        (if (not (null v))
                                            v
                                            2100)))))
       *mntc*))))

(defun add-to-record (recordio file-arr)
  "uploads the file then adds it to the record"
  ;; refresh record (because previous edits of the record could have
  ;; caused it to get out of sync
  (let* ((record (lispcord.http:from-id (lc:id recordio) *mntc*))
         (sha256 (ironclad:digest-sequence :sha256 file-arr))
         (sha256hx (ironclad:byte-array-to-hex-string sha256)))
    (when (not (null (search sha256hx (lc:content record))))
      ;; this means hash collision
      (return-from add-to-record nil))
    (cond ((string= (lc:content record) "placeholder")
           ;; empty record
           (let ((file-head (upload-file file-arr)))
             (when (null file-head)
               (error "File upload failed"))
             (lispcord.http:edit (format nil "~a ~x" sha256hx file-head) record)))
          ((<= (+ 110 (length (lc:content record))) 2000)
           ;; we can safely append here. Hash is 64 long, plus 17
           ;; for ID of b64 file dump, plus 29 for hypothetical NEXT
           (let ((file-head (upload-file file-arr)))
             (when (null file-head)
               (error "File upload failed"))
             (lispcord.http:edit (concatenate 'string
                                              (str:replace-all (format nil "~%") "\\n" (lc:content record))
                                              (format nil "\\n~a ~x" sha256hx file-head))
                                 record)))
          ((not (null (search "NEXT" (lc:content record))))
           ;; this means this record is full (previous check didn't
           ;; succeed) and there exists a NEXT, go to NEXT
           (add-to-record (next-record record) file-arr))
          (t
           ;; this means search for NEXT failed and we need to create a new NEXT
           (let* ((file-head (upload-file file-arr))
                  (new-record (lispcord.http:create (format nil "~a ~x" sha256hx file-head) *mntc*)))
             (lispcord.http:edit (concatenate 'string
                                              (str:replace-all (format nil "~%") "\\n" (lc:content record))
                                              (format nil "\\nNEXT ~x" (lc:id new-record)))
                                 record))))
    sha256hx))

(defmethod put ((path pathname))
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (put s)))

(defmethod put ((stream stream))
  "upload file into filesystem, and then return the hash at which this
  is stored, or NIL on error"
  (let ((arr (make-array 2048 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop while (listen stream)
       do (vector-push-extend (read-byte stream) arr))
    (add-to-record (resolve-to-record arr :file) arr)))

(defmethod put ((arr array))
  "upload array into filesystem, and then return the hash at which this
  is stored, or NIL on error"
  (add-to-record (resolve-to-record arr :file) arr))

(defun remove-from-record (recordio hash-str)
  "remove given hash from the record"
  (let ((record (lispcord.http:from-id (lc:id recordio) *mntc*))
        (hash-st (string-downcase hash-str))
        (done))
    (lispcord.http:edit
     (funcall
      #'(lambda ()
          (eval
           `(concatenate 'string
                         ,@(map 'list
                                #'(lambda (z)
                                    (concatenate 'string z "\\n"))
                                (remove-if
                                 #'(lambda (s)
                                     (if (not (null (search hash-st s)))
                                         (setf done t)
                                         nil))
                                 (split-sequence:split-sequence #\Newline (lc:content record))))))))
     record)
    (when (not done)
      (if (not (null (search "NEXT" (lc:content record))))
          (remove-from-record (next-record record) hash-st)
          nil))))

(defun del (hash-str)
  "delete file with given hash from filesystem, returning T on delete,
  NIL on fail"
  (if (not (null (remove-from-record (resolve-to-record hash-str :hash)
                                     hash-str)))
      t
      nil))

(defun pull-buf (message)
  "given the head of a file, return '(unsigned-byte 8) array of file"
  (let ((acc))
    (labels ((f (msg)
               (let ((spt (split-sequence #\newline (lc:content msg))))
                 (push (nth 0 spt) acc)
                 (when (consp (cdr spt))
                   (f (lispcord.http:from-id (hex-to-snowflake (substring 5 100 (nth 1 spt)))
                                             *mntc*))))))
      (f message))
    (qbase64:decode-string (eval `(concatenate 'string ,@(nreverse acc))))))

(defun get (hash-str)
  "get binary file from filesystem. returns '(unsigned-byte 8) array"
  (labels ((f (record)
             (let ((i (search hash-str record)))
               (cond
                 ((not (null i))
                  (pull-buf
                   (lispcord.http:from-id (hex-to-snowflake
                                           (nth 1 (split-sequence:split-sequence
                                                   #\Space
                                                   (find-if #'(lambda (s) (string= hash-str s :end2 64))
                                                            (split-sequence:split-sequence #\Newline (lc:content record))))))
                                          *mntc*)))
                 ((not (null (search "NEXT" (lc:content record))))
                  (f (next-record record)))))))
    (f (resolve-to-record hash-str :hash))))

(defun start ()
  (connect *discfs*))
