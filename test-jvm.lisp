(defpackage jvm.jvm-test
  (:use common-lisp
        jvm.unit-test
        jvm)
  (:nicknames jvm.jvm-test)
  (:export ))

(in-package jvm.jvm-test)

(deftest test-d ()
  (macrolet ((def (name v)
               `(let ((vm (make-instance 'jvm::virtual-machine)))
                  (,name vm nil)
                  (equal ,v (jvm::current-stack vm)))))
    (check
     (def jvm::dconst-0 '(0 0))
     (def jvm::dconst-1 '(1072693248 0)))))

(deftest test-i ()
  (macrolet ((def (name v)
               `(let ((vm (make-instance 'jvm::virtual-machine)))
                  (,name vm nil)
                  (equal ,v (jvm::current-stack vm)))))
    (check
     (def jvm::iconst-m1 '(-1))
     (def jvm::iconst-0 '(0))
     (def jvm::iconst-1 '(1))
     (def jvm::iconst-2 '(2))
     (def jvm::iconst-3 '(3))
     (def jvm::iconst-4 '(4))
     (def jvm::iconst-5 '(5)))))

(deftest test-l ()
  (macrolet ((def (name v)
               `(let ((vm (make-instance 'jvm::virtual-machine)))
                  (,name vm nil)
                  (equal ,v (jvm::current-stack vm)))))
    (check
     (def jvm::lconst-0 '(0 0))
     (def jvm::lconst-1 '(0 1)))))

(deftest test-f ()
  (macrolet ((def (name v)
               `(let ((vm (make-instance 'jvm::virtual-machine)))
                  (,name vm nil)
                  (equal ,v (jvm::current-stack vm)))))
    (check
     (def jvm::fconst-0 '(0))
     (def jvm::fconst-1 '(1065353216))
     (def jvm::fconst-2 '(2)))))

(deftest test-->words ()
  (check (equal (jvm::->words -1.0f0) '(3212836864))
         (equal (jvm::->words 0.0f0) '(0))
         (equal (jvm::->words -0.0f0) '(2147483648))
         (equal (jvm::->words 1.0f0) '(1065353216))
         (equal (jvm::->words 2.0f0) '(1073741824))
         (equal (jvm::->words -1.0) '(3220176896 0))
         (equal (jvm::->words 0.0) '(0 0))
         (equal (jvm::->words -0.0) '(2147483648 0))
         (equal (jvm::->words 1.0) '(1072693248 0))
         (equal (jvm::->words 2.0) '(1073741824 0))))

(deftest test-words->single-float ()
  (check (eq (jvm::words->single-float '(3212836864)) -1.0f0)
         (eq (jvm::words->single-float '(0)) 0.0f0)
         (eq (jvm::words->single-float '(2147483648)) -0.0f0)
         (eq (jvm::words->single-float '(1065353216)) 1.0f0)
         (eq (jvm::words->single-float '(1073741824)) 2.0f0)))

(deftest test-->single-float ()
  (check (eq (jvm::->single-float '(0 0 0 0)) 0.0f0)
         (eq (jvm::->single-float '(128 0 0 0)) -0.0f0)))

(deftest test-words->double-float ()
  (check (eql (jvm::words->double-float '(3220176896 0)) -1.0)
         (eql (jvm::words->double-float '(0 0)) 0.0)
         (eql (jvm::words->double-float '(2147483648 0)) -0.0)
         (eql (jvm::words->double-float '(1072693248 0)) 1.0)
         (eql (jvm::words->double-float '(1073741824 0)) 2.0)))
