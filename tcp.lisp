;;; some naive TCP networking using usocket.

;; start the game for N players. players join the server and are given
;; a player id 0 - 3. controls are mapped to network streams or
;; keyboards.

(in-package :bratwurst)

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

(defvar *server* nil)

(defun server-p (&optional (server *server*))
  "are we the server?"
  (typep server 'sv-server))

(defun dedicated-server-p (&optional (server *server*))
  "are we the server?"
  (and (server-p server)
       (sv-server-dedicated-p server)))

(defun client-p (&optional (server *server*))
  "are we a client?"
  (typep server 'cl-server))

(defun start-server (free-controls dedicated-p port)
  (setf *server* (make-sv-server :socket (usocket:socket-listen usocket:*wildcard-host* port :backlog 4)
				 :free-controls free-controls
				 :clients nil
				 :dedicated-p dedicated-p))
  (format t "Waiting for connections on ~S:~D~%"
	  (usocket:get-local-name (sv-server-socket *server*))
	  (usocket:get-local-port (sv-server-socket *server*)))
  *server*)

(defun close-server ()
  (usocket:socket-close (sv-server-socket *server*)))
  
(defun sv-send-packet-to (client packet)
  (format (sv-client-socket client) "~s" packet)
  (finish-output (sv-client-socket server)))

(defun sv-send-packet (server packet)
  (loop for i in (sv-server-clients server) do
       (sv-send-packet-to i packet)))

(defun sv-send-join (server controls)
  (sv-send-packet server `(:join ,controls)))

(defun sv-send-selections (client selections)
  (sv-send-packet-to client `(:others ,@selections)))

(defun sv-send-controls (server frame)
  ;;(format t "Sent controls packet ~d~%" frame)
  (sv-send-packet server `(:controls ,frame
				     ,(controls-list (aref *controls* 0))
				     ,@(loop for i in (sv-server-clients server)
					  collect (controls-list (sv-client-controls i))))))

(defun sv-send-heartbeat (server frame)
  (sv-send-packet server `(:heartbeat ,frame)))

(defun sv-send-game-start (server)
  (sv-send-packet server `(:game-start)))

(defun sv-check-for-new-clients (server selections)
  (when (listen (sv-server-socket server))
    (let* ((socket (usocket:socket-accept (sv-server-socket server)))
	   (controls (pop (sv-server-free-controls server)))
	   (client (make-sv-client
		    :socket socket
		    :controls controls)))
      ;; tell them about it
      (format t "Client connecting from ~S:~D to ~S:~D~%"
              (usocket:get-peer-name socket) (usocket:get-peer-port socket)
              (usocket:get-local-name socket) (usocket:get-local-port socket))
      ;; tell the joining player about everyone else
      (sv-send-selections client selections)
      (push client (sv-server-clients server))
      ;; tell all the players about the join.
      (sv-send-join server controls)
      controls)))
    
(defun import-controls (c data)
  (when data
    (setf (controls-left c) (first data)
	  (controls-right c) (second data)
	  (controls-forward c) (third data)
	  (controls-special c) (fourth data)
	  (controls-shoot c) (fifth data))))

(defun sv-handle-client-packet (client packet)
  (let ((type (first packet))
	(data (rest packet)))
    (ecase type
      (:heartbeat
       ;; the client did nothing so they just need to send a heartbeat
       (setf (sv-client-frame-packet-read-p client) t))
      (:controls
       ;; the client pressed or released keys so here's the current
       ;; state of the controls
       (let ((c (sv-client-controls client)))
	 (import-controls c data)
	 (setf (sv-client-frame-packet-read-p client) t))))))

(defun sv-read-client-packets (server client)
  "packets are lists. car is the id and the rest is data."
  (declare (ignore server))
  (when (listen (sv-client-socket client))
    (handler-case
        (sv-handle-client-packet client (read (sv-client-socket client)))
      (end-of-file ()
        (let ((socket (sv-client-socket client)))
          (format t "Client from ~S:~D Disconnected!~%"
                  (usocket:get-peer-name socket) (usocket:get-peer-port socket))
          (setf (sv-client-disconnected-p client) t)
          nil)))))

(defun sv-read-packets (server)
  (loop for i in (sv-server-clients server) do
       (sv-read-client-packets server i)))

(defun sv-read-all-packets (server)
  (loop for i in (sv-server-clients server) do
       (loop while (sv-read-client-packets server i))))

(defun cl-send-heartbeat (server)
  (write-string "(:heartbeat)" (cl-server-socket server))
  (finish-output (cl-server-socket server)))

(defun cl-send-controls (server)
  (let ((c (cl-server-controls server)))
    (format (cl-server-socket server) "(:controls ~a ~a ~a ~a ~a)"
	    (controls-left c)
	    (controls-right c)
	    (controls-forward c)
	    (controls-special c)
	    (controls-shoot c))
    (finish-output (cl-server-socket server))))

(defun cl-handle-server-packet (server packet)
  (let ((type (first packet))
	(data (rest packet)))
    (ecase type
      ;; nothing has changed. so you just get a heartbeat
      (:heartbeat
       ;;(format t "heartbeat for frame ~d!~%" (first data))
       (setf (cl-server-last-frame server) (first data)))
      ;; someone joined the game. data contains the controls index
      (:join 
       (push (first data) (cl-server-players server)))
      ;; the state of the others when this client joined. this is set
      ;; and then bubbles up to the choose ship part.
      (:others
       (setf (cl-server-others server) data))
      ;; an update to 1 or more of the controls
      (:controls
       ;; data will always have up to 4 elements corresponding to each
       ;; player. if its nil then there were no changes to that player's controls
       ;; the second arg is the frame number
       ;;(format t "controls for frame ~d!~%" (first data))
       (setf (cl-server-last-frame server) (pop data))
       (loop
	  for i in data do
	  (import-controls (aref *controls* (car i)) (cdr i))))
      ;; this really seems like the wrong place to do this. 
      (:game-start
       (throw 'done t)))))

(defun cl-read-packets (server)
  (when (listen (cl-server-socket server))
    (cl-handle-server-packet server (read (cl-server-socket server)))
    t))

(defun cl-blocking-read-packets (server)
  (cl-handle-server-packet server (read (cl-server-socket server))))

(defun cl-start-client (host controls port)
  (format t "Attempting to connect to server ~S:~D~%" host port)
  (setf *server* (make-cl-server :socket (usocket:socket-connect port host)
				 :controls controls
				 :players nil))
  (format t "Connected!~%")
  *server*)
