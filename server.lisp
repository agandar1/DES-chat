;;;; server.lisp
;;;; This file defines the server part of our chat application
;;;; It only connets to our clients,
;;;; but does not have a key or decrypt anything

(in-package #:server)

(defparameter *host* "127.0.0.1")
(defparameter *port* 19205)

(defstruct client stream username)
(let ((clients (list)))
  (defun add-client (client) (setf clients (cons client clients)))
  (defun del-client (client) (setf clients (remove client clients)))
  (defun get-clients   (fun) (mapcar fun clients))
  (defun reset-clients    () (setf clients (list))))

 (defun send-msg (client msg)
   (let ((stream (client-stream client)))
     (handler-case (progn (format stream "~a~%" msg)
                          (force-output stream))
       (sb-int:broken-pipe () (del-client client)))))

(defun broadcast (client msg)
  (loop with username = (client-username client)
        for c in (get-clients #'identity) do
          (send-msg c username)
          (send-msg c msg)))

(defun login (stream)
  (flet ((send (x) (format stream x) (force-output stream)))
    (loop for line = (read-line stream nil)
          for current-users = (get-clients #'client-username)
          while line do
            (if (member line current-users :test #'equal)
                (progn (send "NO~%") (force-output stream))
                (progn (send "OK~%") (return line))))))

(defun client-handler (stream)
  (let ((client (make-client :stream stream :username (login stream))))
    (add-client client)
    (loop for line = (read-line stream nil)
          while line do (broadcast client line)
          finally (del-client client))))

(defun server (host port)
  (reset-clients)
  (usocket:socket-server host port #'client-handler nil :multi-threading t))

(defun main ()
  (multiple-value-bind (host port)
      (des-utils:parse-address (car (uiop:command-line-arguments)))
    (handler-case (server host port)
      (sb-sys:interactive-interrupt ()
        (uiop:quit 0))
      (usocket:unknown-error ()
        (uiop:quit 0)))))
