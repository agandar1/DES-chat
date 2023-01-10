;;;; client.lisp
;;;; This file defines the client part of our chat application
;;;; This is the only place we will use our DES encryption and decryption

(in-package #:client)

(defparameter *host* "127.0.0.1")
(defparameter *port* 19205)
(defparameter *encryption-key* "super-secret")

(defun output (stream fstring &rest args)
  "output something onto the command line"
  (apply #'format stream fstring args)
  (if (equal stream t) (finish-output) (finish-output stream)))

(defun clear-last-line ()
  "clear last line with ascii escape codes"
  (if (equal (software-type) "Linux")
      (output t "~C[1A~C[2K" #\Esc #\Esc)))

(defun send-msg (msg socket)
  "send an encrypted message to the server"
  (let ((stream (usocket:socket-stream socket)))
    (output stream "~a~%" (des:encrypt msg :key *encryption-key*))))

(defun login (socket)
  "ask the user for a username until they pick a valid one"
  (output t "Welcome to DES-Chat! You can type /quit to exit.")
  (loop named outer do
    (output t "~&Username: ")
    (let ((username (read-line)))
      (send-msg username socket)
      (usocket:wait-for-input socket)
      (if (equal "OK" (read-line (usocket:socket-stream socket)))
          (progn (send-msg "-----[JOINED THE CHAT]-----" socket)
                 (return-from outer))
          (output t "Username Taken or Invalid.~&")))))

(defun client-listen (socket)
  "listen for, decrypt, and display messages from the server"
  (loop with stream = (usocket:socket-stream socket)
        for user = (des:decrypt (read-line stream nil) :key *encryption-key*)
        for msg = (des:decrypt (read-line stream nil) :key *encryption-key*)
        while msg do
          (output t "~10<~a~> | ~a~%" user msg)))

(defun sender (socket)
  "loop for getting messages from the user and sending them to the server"
  (loop for msg = (read-line)
        while (not (equal msg "/quit")) do
          (clear-last-line)
          (send-msg msg socket)
        finally (clear-last-line)
                (send-msg "-----[LEFT THE CHAT]-----" socket)
                (sleep 0.5)
                (usocket:socket-close socket)
                (uiop:quit 0)))

(defun reciever (socket)
  "call the client-listen function with error handling"
  (handler-case (client-listen socket)
    (end-of-file (e)
      (output t "~%Connection Terminated~%" e)
      (uiop:quit 1))))

(defun client (host port)
  "connect to the client, log in and start the send/recieve loops in separate threads"
  (let* ((socket  (usocket:socket-connect host port))
         (name    (login socket))
         (send    (bt:make-thread (lambda () (sender socket)) :name "sender"))
         (recieve (bt:make-thread (lambda () (reciever socket) :name "reciever"))))
    (bt:join-thread send) (bt:join-thread recieve)))

(defun main ()
  "main function. get host and port from command line args and start the client"
  (multiple-value-bind (host port)
      (des-utils:parse-address (car (uiop:command-line-arguments)))
    (handler-case (client host port)
      (sb-sys:interactive-interrupt ()
        (uiop:quit 0))
      (usocket:connection-refused-error ()
        (progn (output t "~%Could not connect to server.")
               (output t "~&Make sure server is running and address is correct.") 
               (output t "~&example:  windows: client.exe \"localhost:8080\"")
               (output t "~&            linux: ./client \"localhost:8080\"~%~%")
               (uiop:quit 1))))))
