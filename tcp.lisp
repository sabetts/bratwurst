;;; some naive TCP networking using usocket.

;; start the game for N players. players join the server and are given
;; a player id 0 - 3. controls are mapped to network streams or
;; keyboards.

(in-package :bratwurst)

#+lispworks
(defun socket-status (socket)
  (comm::socket-listen socket))

#+sbcl
(defun socket-status (socket)
  (sb-alien:with-alien ((rfds (sb-alien:struct sb-unix:fd-set)))
    (sb-unix:fd-zero rfds)
    (sb-unix:fd-set
     (sb-bsd-sockets:socket-file-descriptor socket)
     rfds)
    (multiple-value-bind (count err)
        (sb-unix:unix-fast-select
         (1+ (sb-bsd-sockets:socket-file-descriptor socket))
         (sb-alien:addr rfds) nil nil
         0 0)
      (unless (zerop err)
        (error "ahhh fuck ~a" err))
      (plusp count))))
        
(defstruct cl-server
  socket controls
  ;; a list of indexes into *controls* for the other players. this is
  ;; used when choosing the stage to detect new players
  players
  ;; a list of the existing player choose states when this client
  ;; joined
  others
  ;; the last frame from the server
  last-frame)

(defstruct sv-client
  frame-packet-read-p
  socket controls stream
  ;; this is set to T when we get they close their connection
  disconnected-p)

(defstruct sv-server
  socket
  free-controls
  clients
  dedicated-p)

(defun server-p (server)
  "are we the server?"
  (typep server 'sv-server))

(defun dedicated-server-p (server)
  "are we the server?"
  (and (server-p server)
       (sv-server-dedicated-p server)))

(defun client-p (server)
  "are we a client?"
  (typep server 'cl-server))

(defun start-server (free-controls dedicated-p port)
  (let ((server (handler-case
                    (make-sv-server :socket (usocket:socket-listen usocket:*wildcard-host* port :backlog 4)
                                    :free-controls free-controls
                                    :clients nil
                                    :dedicated-p dedicated-p)
                  (usocket:socket-error (c)
                    (format t "error starting server: ~a~%" c)
                    nil))))
  (when server
    (format t "Waiting for connections on ~S:~D~%"
            (usocket:get-local-name (sv-server-socket server))
            (usocket:get-local-port (sv-server-socket server)))
    server)))

(defun close-server (server)
  (mapc 'usocket:socket-close (mapcar 'sv-client-socket (sv-server-clients server)))
  (usocket:socket-close (sv-server-socket server)))
  
(defun sv-send-packet-to (client packet)
  (format (usocket:socket-stream (sv-client-socket client)) "~s~%" packet)
  (force-output (usocket:socket-stream (sv-client-socket client))))

(defun sv-send-packet (server packet)
  (loop for i in (sv-server-clients server) do
       (sv-send-packet-to i packet)))

(defun sv-send-game-start (server)
  (sv-send-packet server `(:game-start)))

(defun sv-check-for-new-clients (server)
  (when (socket-status (usocket:socket (sv-server-socket server)))
    (let* ((socket (usocket:socket-accept (sv-server-socket server)))
	   (controls (pop (sv-server-free-controls server)))
	   (client (make-sv-client
		    :socket socket
		    :controls controls)))
      ;; tell them about it
      (format t "Client connecting from ~S:~D to ~S:~D~%"
              (usocket:get-peer-name socket) (usocket:get-peer-port socket)
              (usocket:get-local-name socket) (usocket:get-local-port socket))
      (push client (sv-server-clients server))
      client)))
    
(defun import-controls (c data)
  (when data
    (dolist (i '(left right forward special shoot))
      (setf (slot-value c i) (slot-value data i)))))

(defun sv-read-client-packet (server client)
  "packets are lists. car is the id and the rest is data."
  (declare (ignore server))
  (when (socket-status (usocket:socket (sv-client-socket client)))
    (handler-case
        (let ((*package* (find-package :bratwurst)))
          (read (usocket:socket-stream (sv-client-socket client))))
      (end-of-file ()
        (let ((socket (sv-client-socket client)))
          (format t "Client from ~S:~D Disconnected!~%"
                  (usocket:get-peer-name socket) (usocket:get-peer-port socket))
          (setf (sv-client-disconnected-p client) t)
          nil)))))

(defmacro with-sv-packets ((server) &body body)
  `(loop
      for %client% in (sv-server-clients ,server)
      do (loop for %packet% = (sv-read-client-packet ,server %client%)
            while %packet%
            do (ecase (first %packet%)
                 ,@body))))

;;; Client

(defun cl-send-controls (server c)
  (format (usocket:socket-stream (cl-server-socket server)) "(:controls ~s)~%" c)
  (force-output (usocket:socket-stream (cl-server-socket server))))

(defun cl-read-packet (server)
  (when (socket-status (usocket:socket (cl-server-socket server)))
    (handler-case
        (let ((*package* (find-package :bratwurst)))
          (read (usocket:socket-stream (cl-server-socket server))))
      (end-of-file ()
        `(:disconnect)))))

(defmacro with-cl-packets ((server) &body body)
  `(loop for %packet% = (cl-read-packet ,server)
            while %packet%
            do (ecase (first %packet%)
                 ,@body)))

(defun cl-start-client (host controls port)
  (format t "Attempting to connect to server ~S:~D~%" host port)
  (let ((socket (handler-case
                    (usocket:socket-connect host port #|:timeout 10|#)
                  (usocket:ns-error (c)
                    (format t "error resolving host: ~a~%" c))
                  (usocket:socket-error (c)
                    (format t "error connecting: ~a~%" c)
                    nil))))
    (when socket
      (format t "Connected!~%")
      (make-cl-server :socket socket
                      :controls controls
                      :players nil))))

(defun close-client (server)
  (usocket:socket-close (cl-server-socket server)))
