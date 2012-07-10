(in-package jvm)
(shadow '(return pop type))

(defun ->single-float (l)
  (let* ((a (loop for i from (1- (length l)) downto 0
               for j in l
               sum (* (expt 256 i) j)))
         (sign (ldb (byte 1 31) a))
         (e (ldb (byte 8 23) a))
         (f (loop for i from 22 downto 0
               sum (* (ldb (byte 1 i) a) (expt 2 (- i 23))))))
    (coerce 
     (if (zerop e) 
         (if (= sign 1) -0.0f0 0.0f0)
         (* (expt -1 sign) (1+ f) (expt 2 (- e 127))))
     'single-float)))

(defun ->double-float (l)
  (let* ((a (loop for i from (1- (length l)) downto 0
               for j in l
               sum (* (expt 256 i) j)))
         (sign (ldb (byte 1 63) a))
         (e (ldb (byte 11 52) a))
         (f (loop for i from 51 downto 0
               sum (* (ldb (byte 1 i) a) (expt 2 (- i 52))))))
    (coerce 
     (if (zerop e) 
         (if (= sign 1) -0.0 0.0)
         (* (expt -1 sign) (1+ f) (expt 2 (- e 1023))))
     'double-float)))

(defmethod ->words ((obj single-float))
  (multiple-value-bind (significand exponent integer-sign)
      (integer-decode-float obj)
    (let ((a 0))
      (setf (ldb (byte 23 0) a) (ldb (byte 23 0) significand)
            (ldb (byte 8 23) a) (ldb (byte 8 0) (+ 127 (+ 23 exponent)))
            (ldb (byte 1 31) a) (ldb (byte 1 0) (if (= 1 integer-sign) 0 1)))
      (list a))))

(defmethod ->words ((obj double-float))
  (multiple-value-bind (significand exponent integer-sign)
      (integer-decode-float obj)
    (let ((a 0))
      (setf (ldb (byte 52 0) a) (ldb (byte 52 0) significand)
            (ldb (byte 11 52) a) (ldb (byte 11 0) (+ 1023 (+ 52 exponent)))
            (ldb (byte 1 63) a) (ldb (byte 1 0) (if (= 1 integer-sign) 0 1)))
      (list (ldb (byte 32 32) a) 
            (ldb (byte 32 0) a)))))

(defmethod ->byte-list ((obj single-float))
  (loop with a = (car (->words obj))
     for i from 3 downto 0
     collect (ldb (byte 8 (* 8 i)) a)))

(defmethod ->byte-list ((obj double-float))
  (append
   (loop with a = (first (->words obj))
      for i from 3 downto 0
      collect (ldb (byte 8 (* 8 i)) a))
   (loop with a = (second (->words obj))
      for i from 3 downto 0
      collect (ldb (byte 8 (* 8 i)) a))))

(defun 2words-int (a b)
  (+ (* #.(expt 2 32) a) b))

(defun 2words-signed-int (a b)
  (let ((v (2words-int a b)))
    (if (= 1 (ldb (byte 1 63) v))
        (- v #.(expt 2 64))
        v)))

(defun words->single-float (words)
  (->single-float 
   (loop for i from 3 downto 0 collect (ldb (byte 8 (* i 8)) (car words)))))

(defun words->double-float (words)
  (->double-float 
   (append (loop for i from 3 downto 0 collect (ldb (byte 8 (* i 8)) (first words)))
           (loop for i from 3 downto 0 collect (ldb (byte 8 (* i 8)) (second words))))))

(defun uniq (l)
  (let ((h (make-hash-table)))
    (dolist (i l)
      (setf (gethash i h) nil))
    (loop for k being the hash-keys in h 
       collect k)))

