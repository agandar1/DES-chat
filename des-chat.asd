;;;; des-chat.asd

(asdf:defsystem #:des-chat
  :description "DES encryption project"
  :author "Adan Gandarilla <adan.gandarilla01@utrgv.edu>"
  :license  "None"
  :version "1.0"
  :serial t
  :depends-on (#:usocket
               #:usocket-server
               #:bt-semaphore)
  :components ((:file "package")
               (:file "des-utils")
               (:file "des-constants")
               (:file "des")
               (:file "server")
               (:file "client")))
