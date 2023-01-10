;;;; utils.lisp
;;;; This file defines general utility functions 
;;;; not necessarily specific to DES encryption
(in-package #:des-utils)


(defun array-map (function array &optional (result (make-array (array-dimensions array))))
  "map FUNCTION across ARRAY of any dimensions"
  (dotimes (i (array-total-size array) result)
    (setf (row-major-aref result i)
          (funcall function (row-major-aref array i)))))

(defun mapconcat (result-type function list &rest lists)
  "map FUNCTION over one or more lists and concatenate all the results into a RESULT-TYPE"
  (apply #'concatenate result-type
         (apply #'map 'list function list lists)))

(defun partition (seq psize)
  "split a sequence into multiple sequences of length PSIZE"
  (loop with len = (length seq)
        for start from 0 by psize below len
        collect (subseq seq start (min len (+ start psize)))))

(defun compose (&rest fns)
  "compose various functions into one"
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(defun half-split (seq)
  "split any sequence in half, and return both halves as separate values"
  (let ((half (ceiling (length seq) 2)))
    (values (subseq seq 0 half) (subseq seq half))))

(defun bits->literal (bits)
  "convert a string of literal bits to a bit-vector, '1010' -> #*1010"
  (map 'string #'digit-char bits))

(defun literal->bits (str)
  "convert a bit-vector to a string of literal bits, #*1010 -> '1010'"
  (map 'bit-vector #'digit-char-p str))

(defun int->bits (int &key (bits 8))
  "convert an integer to a bit-vector length at least BITS"
  (let ((fstring (format nil "~~~a,'0b" bits)))
    (map 'bit-vector #'digit-char-p (format nil fstring int))))

(defun bits->int (bits)
  "convert bit-vector to an integer"
  (reduce (lambda (x y) (+ y (* x 2))) bits))

(defun str->bits (str)
  "convert a string to a bit-vector"
  (mapconcat 'bit-vector (compose #'int->bits #'char-code) str))

(defun bits->str (bits)
  "convert a bit-vector to a string. 8 bits are treated as a character"
  (format nil "~{~a~}"
          (mapcar (compose #'code-char #'bits->int) (partition bits 8))))

(defun pad-bits (bits target-size)
  (let* ((pad1 (concatenate 'bit-vector bits #*1))
         (missing (- target-size (mod (length pad1) target-size))))
    (concatenate 'bit-vector pad1 (make-array missing :element-type 'bit))))

(defun unpad-bits (bits)
  (subseq bits 0 (position 1 bits :from-end t)))

(defun lshift (bits amount)
  "shift BITS circularly to the left by AMOUNT bits"
  (concatenate 'bit-vector (subseq bits amount) (subseq bits 0 amount)))

(defun parse-address (address)
  "parse a tcp ADDRESS, 'localhost:8080' -> (values 'localhost' 8080)"
  (let* ((cut   (1+ (or (position #\/ address :from-end t) -1)))
         (split (uiop:split-string (subseq address cut) :separator ":")))
    (values (car split) (parse-integer (cadr split)))))
