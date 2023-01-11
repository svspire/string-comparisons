(in-package :string-comparisons)

(defmacro dotest (form desired-result)
  (let ((resultvar (gensym)))
    `(let ((,resultvar ,form))
       (if (eql t ,desired-result)
           (or ,resultvar
             (error "~S failed. Result was ~S; should have been true." ',form ,resultvar))
           (or (eql ,desired-result ,resultvar)
             (error "~S failed. Result was ~S; should have been ~S." ',form ,resultvar ,desired-result))))))

(defparameter *notorious-6* (coerce #(#\[ #\\  #\] #\^ #\_ #\`) 'string))

(defun do-tests ()
  (dotest (std-char-equal #\A #\a) t)
  (dotest (std-string-equal "A" "a") t)
  
  (dotest (std-char-lessp #\a #\[) t)
  (dotest (std-char-lessp #\A #\[) t)
  (dotest (std-string-lessp "a" "[") 0)
  (dotest (std-string-lessp "A" "[") 0)
  
  (dotest (std-char-not-greaterp #\A #\[) t)
  (dotest (std-string-not-greaterp "a" "[") 0)
  (dotest (std-string-not-greaterp "A" "[") 0)
  
  (dotest (std-char-not-greaterp #\a #\A) t)
  (dotest (std-char-not-greaterp #\A #\a) t)
  (dotest (std-char-not-greaterp #\a #\[) t)
  (dotest (std-char-not-greaterp #\[ #\a) nil)
  
  (dotest (std-string-not-greaterp "a" "A") 1)
  (dotest (std-string-not-greaterp "A" "a") 1)
  
  (dotest (std-string-equal "foo" "Foo") t)
  
  (dotest (std-string-not-greaterp "Abcde" "abcdE") 5)
  (dotest (std-string-lessp "Abcde" "abcdE") nil)
  (dotest (std-string-lessp "abcdE" "Abcde") nil)
  
  (dotest (std-string-lessp "abcd" "Abcde") 4)
  (dotest (string-lessp "Abcde" "abcd") nil)
  
  (dotest (std-string-lessp "012AAAA789" "01aaab6" :start1 3 :end1 7
                            :start2 2 :end2 6) 6)
  
  (dotest (std-string-not-equal "AAAA" "aaaA") nil)
  
  (dotest (strcomp::std-string-lessp
           (concatenate 'string "A" strcomp::*notorious-6*)
           (concatenate 'string "b" strcomp::*notorious-6*))
          0)
  
  (dotest (strcomp::std-string-lessp 
           (concatenate 'string "a" strcomp::*notorious-6*)
           (concatenate 'string "B" strcomp::*notorious-6*))
          0)
  
  (dotest (strcomp::std-string-lessp
           (concatenate 'string "b" strcomp::*notorious-6*)
           (concatenate 'string "A" strcomp::*notorious-6*))
          nil)
  
  (dotest (strcomp::std-string-lessp 
           (concatenate 'string "B" strcomp::*notorious-6*)
           (concatenate 'string "a" strcomp::*notorious-6*))
          nil)
  )

;;; (do-tests)