;;; initialize
;; server
(defparameter my-socket (socket-server 4321))
(defparameter my-stream (socket-accept my-socket))

;; client
(defparameter my-stream (socket-connect 4321 "127.0.0.1"))


;;; client => server
;; client
(print "Yo Server!" my-stream)
"Yo Server!"

;; server
(read my-stream)
;; "Yo Server!"


;;; server => client
;; server
(print "What up, Client!" my-stream)
;; "What up, Client!"

;; client
(read my-stream)
;; "What up, Client!"



;;; finalize
;; client
(close my-stream)
;; T

;; server
(close my-stream)
;; T
(socket-server-close my-socket)
;; NIL

