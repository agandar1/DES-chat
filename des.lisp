;;;; des.lisp
;;;; This file defines the actual DES algorithm
(in-package #:des)


(defun permute (bits table)
  "apply the permutation table TABLE to bit-vector BITS"
  (map 'bit-vector (lambda (x) (bit bits (1- x))) table))

(defun gen-subkeys (key-bits)
  "generate the 16 48bit binary subkeys from bit-vector KEY-BITS"
  (multiple-value-bind (c0 d0) (half-split (permute key-bits +pc-1+))
    (loop for x across +shift-table+
          for c = (lshift c0 x) then (lshift c x)
          for d = (lshift d0 x) then (lshift d x)
          collect (permute (concatenate 'bit-vector c d) +pc-2+))))

(defun sbox (bits box)
  "convert 6 bits to 4 bits using the s-box SBOX"
  (let ((row (+ (bit bits 5) (* 2 (bit bits 0))))
        (col (bits->int (subseq bits 1 5))))
    (int->bits (aref box row col) :bits 4)))

(defun des-f (r-block key)
  "function F for DES encryption, including E expansion and S-boxes"
  (let* ((e      (permute r-block +e-table+))
         (groups (partition (bit-xor key e) 6))
         (s-box  (mapconcat 'bit-vector #'sbox groups +s-boxes+)))
    (permute s-box +p-table+)))

(defun des-round (l-block r-block key)
  "run one round of DES encryption/decryption given left and right 32bit blocks"
  (values r-block (bit-xor l-block (des-f r-block key))))

(defun des (block key &key (decrypt nil))
  "pass a 64bit binary block through DES using key KEY (binary) to encrypt or decrypt"
  (multiple-value-bind (l0 r0) (half-split (permute block +ip-table+))
    (let* ((subkeys (gen-subkeys key))
           (keys    (if decrypt (reverse subkeys) subkeys)))
      (loop for i from 0 to 15
            for (l r) = (multiple-value-list (des-round l0 r0 (nth i keys)))
              then (multiple-value-list (des-round l r (nth i keys)))
            finally (return (permute (concatenate 'bit-vector r l) +ip-table2+))))))

(defun encrypt (msg-str &key (key "a-secret"))
  "encrypt plaintext MSG-STR using DES with string key KEY"
  (let ((key    (pad-bits (str->bits key) 64))
        (blocks (partition (pad-bits (str->bits msg-str) 64) 64)))
    (bits->literal (mapconcat 'bit-vector (lambda (x) (des x key)) blocks))))

(defun decrypt (cipher &key (key "a-secret"))
  "decrypt ciphertext CIPHER using DES with string key KEY"
  (let* ((key    (pad-bits (str->bits key) 64))
         (blocks (partition (literal->bits cipher) 64))
         (plain  (mapconcat 'bit-vector (lambda (x) (des x key :decrypt t)) blocks)))
    (bits->str (unpad-bits plain))))
