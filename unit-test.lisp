(defpackage jvm.unit-test
  (:use common-lisp)
  (:nicknames jvm.unit-test)
  (:export deftest
           close-to
           check
           check-equal
           run-test-suite))

(in-package jvm.unit-test)

(defvar *test-name* nil)
(defvar *test-names* nil)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) (list s '(gensym))) syms)
     ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* ,name))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defun close-to (expected value accuracy)
  (< (abs (- expected value)) accuracy))

(defun run-test-suite ()
  (dolist (name *test-names*)
    (funcall name)))
