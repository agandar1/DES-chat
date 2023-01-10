;;;; package.lisp

(defpackage #:des-utils
  (:use #:cl)
  (:export :array-map
           :mapconcat
           :compose
           :partition
           :half-split
           :int->bits
           :bits->int
           :str->bits
           :bits->str
           :literal->bits
           :bits->literal
           :pad-bits
           :unpad-bits
           :lshift
           :parse-address))

(defpackage #:des-constants
  (:use #:cl)
  (:nicknames :const)
  (:export :+ip-table+
           :+ip-table2+
           :+e-table+
           :+pc-1+
           :+pc-2+
           :+shift-table+
           :+p-table+
           :+s-boxes+))

(defpackage #:des
  (:use #:cl #:des-utils #:des-constants)
  (:export :encrypt
           :decrypt))

(defpackage #:client
  (:use #:cl))

(defpackage #:server
  (:use #:cl))
