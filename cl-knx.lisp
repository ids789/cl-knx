;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CL-KNX: A KNX Client Library for LISP ;;
;;       Written by Ian Schipper         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-knx)

(defparameter *recv-buffer-size* 16)

;; Load the KNXD client API
(cffi:define-foreign-library libeibclient
  (t (:default "libeibclient")))
(cffi:use-foreign-library libeibclient)

;; Generate lisp objects from the eibclient c api
(autowrap:c-include
 (asdf:system-relative-pathname
  :cl-knx "spec/eibclient.h")
 :spec-path '(:cl-knx spec))

;; Ensure the KNXD cache is active
(with-connection con"ip:127.0.0.1:9999"
  (eib-cache-enable con))

(defun check-group-valid (group)
  "Check if a KNX group value is valid"
  (and (listp group)
	   (= 3 (length group))
	   (loop :for element :in group :always (numberp element))))

(defun group-encode (group)
  "Convert a group from a list into an eibaddr_t value"
  (let ((l1 (first group))  ; 5 bits
		(l2 (second group)) ; 3 bits
		(l3 (third group))) ; 8 bits
	(if (and (numberp l1) (numberp l2) (numberp l3)
			 (>= l1 0) (>= l2 0) (>= l3 0) (< l1 32) (< l2 8) (< l3 256))
		(logior (ash l1 11) (ash l2 8) l3)
		(error "The KNX group address ~A is invalid" (list l1 l2 l3)))))

(defun group-decode (group)
  "Convert an eibaddr_t value into a group list"
  (list (ash (logand group 63488) -11) ; bits 11 to 16
		(ash (logand group 1792) -8)   ; bits 8 to 11
		(logand group 255)))           ; bits 0 to 8

(defun addr-decode (addr)
  "Convert an eibaddr_t value into an address list"
  (list (ash (logand addr 61440) -12) ; bits 12 to 16
		(ash (logand addr 3840) -8)   ; bits 8 to 12
		(logand addr 255)))           ; bits 0 to 8

(defun msg-decode (message type)
  "Convert received buffer data into a lisp value"
  (check-type message list "a KNX message payload list")
  (case type
	(dpt-1.*
	 (if (= 2 (length message))
		 (if (= 1 (logand (cadr message) 1)) t nil)
		 (error "Incorrect message length for type dpt-1.*: ~A"
				(length message))))
	(dpt-5.001
	 (if (= 3 (length message))
		 (/ (* 100 (float (third message))) 255)
		 (error "Incorrect message length for type dpt-5.001: ~A"
				(length message))))
	(dpt-17.001
	 (if (= 3 (length message))
		 (third message)
		 (error "Incorrect message length for type dpt-17.001: ~A"
				(length message))))
	(dpt-9.*
	 (if (= 4 (length message))
		 (let ((sign (ash (logand #x80 (third message)) -7))
			   (expo (ash (logand #x78 (third message)) -3))
			   (mant (logior (ash (logand #x7 (third message)) 8)
							 (fourth message))))
		   (* 0.01 (expt 2 expo) (+ (* sign -2048) mant)))
		 (error "Incorrect message length for type dpt-9.*: ~A"
				(length message))))
	(dpt-14.*
	 (if (= 6 (length message))
		 (with-foreign-object (val :float)
		   (setf (mem-aref val :unsigned-char 3) (third message))
		   (setf (mem-aref val :unsigned-char 2) (fourth message))
		   (setf (mem-aref val :unsigned-char 1) (fifth message))
		   (setf (mem-aref val :unsigned-char 0) (sixth message))
		   (mem-aref val :float))
		 (error "Incorrect message length for type dpt-14.*: ~A"
				(length message))))
	(dpt-13.*
	 (if (= 6 (length message))
		 (with-foreign-object (val :int32)
		   (setf (mem-aref val :unsigned-char 3) (third message))
		   (setf (mem-aref val :unsigned-char 2) (fourth message))
		   (setf (mem-aref val :unsigned-char 1) (fifth message))
		   (setf (mem-aref val :unsigned-char 0) (sixth message))
		   (mem-aref val :int32))
		 (error "Incorrect message length for type dpt-13.*: ~A"
				(length message))))
	 (t (error "Unknown message type ~A" type))))

(defun msg-encode (value type)
  "Convert a lisp value into a message buffer"
  (case type
	(dpt-1.*
	 (list #x00 (if value #x81 #x80)))
	(dpt-5.001
	 (if (and (numberp value) (>= value 0) (<= value 100))
		 (list #x00 #x80 (floor (/ (* 255 value) 100)))
		 (error "Invalid value for type dpt-5.001: '~A'" value)))
	(dpt-17.001
	 (if (and (numberp value) (> value 0) (< value 256))
		 (list #x00 #x80 (- value 1))
		 (error "Invalid value for type byte dpt-17.001: '~A'" value)))
	(dpt-9.*
	 (if (numberp value)
		 ;; loop until an exponent value is found where the mantissa is
		 ;; less than the maxiumum 11 bits (2048)
		 (do* ((exp -1 (1+ exp))
			   (mant 2048 (/ (* 100.0 (abs value)) (expt 2 exp)))) 
			  ((< mant 2048)
			   (if (> exp 15)
				   (error "Value is too large for dpt-9.*: ~A" value)
				   (let ((sign (if (>= value 0) 0 1))
						 (mant* (if (>= value 0)
									mant
									(- 2048 mant))))
					 (list #x00 #x80
						   (logior (ash sign 7)
								   (ash exp 3)
								   (ash (logand #x700 (floor mant*)) -8))
						   (logand #xFF (floor mant*)))))))
		 (error "Invalid value for type dpt-9.*: ~A" value)))
	(dpt-14.*
	 (if (floatp value)
		 (with-foreign-object (val :float)
		   (setf (mem-ref val :float) value)
		   (list #x00 #x80
				 (mem-aref val :unsigned-char 3)
				 (mem-aref val :unsigned-char 2)
				 (mem-aref val :unsigned-char 1)
				 (mem-aref val :unsigned-char 0)))
		 (error "Invalid value type for dpt-14.*: ~A" value)))
	(dpt-13.*
	 (if (integerp value)
		 (with-foreign-object (val :int32)
		   (setf (mem-ref val :int32) value)
		   (list #x00 #x80
				 (mem-aref val :unsigned-char 3)
				 (mem-aref val :unsigned-char 2)
				 (mem-aref val :unsigned-char 1)
				 (mem-aref val :unsigned-char 0)))
		 (error "Invalid value type for dpt-13.*: ~A" value)))
	(t (error "Unknown message type ~A" type))))

(defun knx-connect (url)
  "Open a connection to a KNXD server"
  (let ((con (with-foreign-string (url-obj url)
			   (eib-socket-url url-obj))))
	(if (null-pointer-p (eib-connection-ptr con))
		(error 'knxd-socket-fail "Failed to connect to KNXD")
		con)))

(defun knx-close (con)
  "Close a KNXD connection"
  (if (not (zerop (eib-close con)))
	  (error "Failed to close KNXD connection")))

(defmacro with-connection (con url &body body)
  "Open a KNX connection; do something; close the connection"
  `(let* ((,con (knx-connect ,url))
		  (res (multiple-value-list (progn ,@body))))
	 (knx-close ,con)
	 (apply 'values res)))

(defun knx-send (url group message)
  "Send a message on the KNX bus"
  (check-type url string)
  (assert (check-group-valid group))
  (check-type message list "a KNX message payload list")
  (with-connection con url
	(let ((group-val (group-encode group))
		  (msg (foreign-alloc :unsigned-char
							  :initial-contents message)))
	  (if (not (zerop (eib-open-t-group con group-val 1)))
		  (error "Failed to open group ~A" group))
	  
	  (if (not (= (length message) (eib-send-apdu con (length message) msg)))
		  (error "Failed to send KNX telegram"))
	  
	  (foreign-free msg))))

(defun knx-read (url group &optional (age 0))
  "Request a KNX group's value"
  (check-type url string)
  (assert (check-group-valid group))
  (check-type age integer)
  (with-connection con
	url
	(with-foreign-objects ((source :uint16)
						   (buffer :unsigned-char *recv-buffer-size*))
	  (let ((len (eib-cache-read-sync con (group-encode group) source
									  *recv-buffer-size* buffer age)))
		(if (or (= -1 len) (< len 2))
			(error "KNX Telegram Read Failed"))

		(loop :for i :below len
					 :collect (mem-ref buffer :unsigned-char i))))))

(defun knx-listen (url)
  "Listen for any telegram from the KNX bus"
  (check-type url string)
  (with-connection con url
	(if (not (zerop (eib-open-group-socket con 0)))
		(error "Failed to configure knx communication"))

	(with-foreign-objects ((group :uint16)
						   (source :uint16)
						   (buffer :unsigned-char *recv-buffer-size*))

	  (let ((len (eib-get-group-src con *recv-buffer-size* buffer
									source group)))
		(if (or (= -1 len) (< len 2))
			(error "KNX Telegram Read Failed"))

		;; Return: data, telegram-type, destination-group, source-address
		(values (loop :for i :below len
					  :collect (mem-ref buffer :unsigned-char i))
				(case (mem-aref buffer :unsigned-char 1)
				  (#x00 'request)
				  (#x40 'response)
				  (#x80 'write))
				(group-decode (mem-ref group :uint16))
				(addr-decode (mem-ref source :uint16)))))))
