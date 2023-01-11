;;; string-comparisons.lisp
;;; 10-Jan-2023 SVS

; Copyright (c) 2023, Shannon Spires
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.

;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.

;;   * Neither Shannon Spires nor the names of its contributors of the
;;     software may be used to endorse or promote products derived from
;;     this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Deals with differences in how various Common Lisp implementations
;;;  handle case-insensitive string and character comparisons. This matters in
;;;  the case of the 'notorious 6' characters (see below).

;;; Herein we augment the case-insensitive string and character comparison functions
;;;   with new ones named 'std-' prepended.
;;; We attempt to take advantage of CL implementations that are already doing
;;;   this right without adding overhead, but the functions herein that
;;;   correct for deficiencies in CL implementations that are doing it wrong
;;;   might be slower than the native [wrong] functions.

;;;  In depth discussion:
;;;  The ANSI spec does not specify whether case-insensitive string and character comparisons
;;;  are done by first converting strings/characters to uppercase or to lowercase.
;;;  This matters because of the 'notorious 6' problem: The 6 ASCII characters
;;;  between (char-code #\Z)) and (char-code #\a) are 
;;;  #\[ 
;;;  #\\ 
;;;  #\] 
;;;  #\^ 
;;;  #\_ 
;;;  #\` 
;;;  and these characters will be considered 'lessp' than the alphabetic characters
;;;  in Lisps that do lowercase conversion and 'not lessp' than the alphabetic characters
;;;  in Lisps that do uppercase conversion. This can wreak havoc in systems that depend
;;;  on a single, standard case-insensitive ordering of characters, especially when those systems
;;;  e.g. sort strings with one CL implementation, save the results, and then use those results
;;;  with a different CL implementation.

;;; As of this writing, we have tested four CL implementations. CCL, Lispworks, and ACL do
;;;  prior uppercase conversion while SBCL does prior lowercase conversion. We prefer
;;;  prior uppercase conversion for several reasons:
;;;  --Uppercase conversion means that all of the punctuation characters with codes greater
;;;    than (char-code #\Z) [including the notorious 6] will always appear after the
;;;    [ASCII] alphabetic characters. Lowercase conversion means that punctuation characters fall
;;;    into two groups: The notorious 6 which are before the [ASCII] alphabetic characters,
;;;    and the other group #\{, #\|, #\}, and #\~ which are after them.
;;;    Comment: This is a fairly minor consideration given that a number of punctuation
;;;     characters appear before (char-code #\0) and those are always going to be in a separate
;;;     group below the alphabetic characters. So uppercase conversion means punctuation falls
;;;     into two separate groups while lowercase conversion means they fall into three.
;;;  --Our primary platforms are CCL and Lispworks so we're biased by the way they do things.
;;;  --Of the four major CL platforms, only SBCL is the outlier, so we choose to call the others
;;;    standard and write special-purpose code for SBCL (and any other implementation that does
;;;    character comparisons the way SBCL does.)

;;; The code herein should automatically work for any CL implementation -- not just the four
;;;   named above -- because it checks the actual functionality of the implementation.

;;; The code herein should have no effect on Unicode character comparisons when either char-code is > #x7F.
;;;   It should only make a difference when comparing pure-ASCII characters between codes #x5B and #x60 (inclusive)
;;;   to ASCII alphabetic characters.

;;; We assume that #'string-upcase is correctly implemented in all CL implementations. We make no attempt here
;;;   to test that assumption.

;;; CL functions that are augmented herein: (Note: Not redefined! Originals are still available if needed.)
;;;    char-equal        --> std-char-equal [see Note 1]
;;;    char-not-equal    --> std-char-not-equal [see Note 1]
;;;    char-lessp        --> std-char-lessp
;;;    char-greaterp     --> std-char-greaterp
;;;    char-not-greaterp --> std-char-not-greaterp
;;;    char-not-lessp    --> std-char-not-lessp

;;;    string-equal        --> std-string-equal [see Note 1]
;;;    string-not-equal    --> std-string-not-equal [see Note 1]
;;;    string-lessp        --> std-string-lessp
;;;    string-greaterp     --> std-string-greaterp
;;;    string-not-greaterp --> std-string-not-greaterp
;;;    string-not-lessp    --> std-string-not-lessp


;;; Note 1: We're not actually adding any value to the equality functions herein because _I don't think_ it matters whether
;;;      those functions call char-upcase or char-downcase. The others matter because the choice affects the ordering of characters.
;;;      But we're assigning the equality functions 'std-' names purely for consistency with the rest of the case-insensitive functions
;;;      herein.


(in-package :string-comparisons)

(defun test-implementation ()
  "Returns either :tolower or :toupper or :mixed
   depending on whether the underlying Common Lisp implementation
   converts characters to lower or upper case prior to doing
   comparisons.
   (:toupper is our preferred result for standardization purposes herein.)
   :mixed means that the implementation handles #'char-lessp differently
   than #'string-lessp."
  (let ((string-result (string-lessp "[" "a"))
        (char-result (char-lessp #\[ #\a)))
    (cond ((and string-result
                char-result)
           :tolower)
          ((and (not string-result)
                (not char-result))
           :toupper)
          (t :mixed))))

(eval-when (:compile :load :execute)
  (unless (eq :toupper (test-implementation)) ; If a :mixed implementation occurs, treat it like :tolower
    (pushnew :string-comparison-punt *features*)))

;;; Some code below cribbed from CCL source. Should be reasonably fast.
#+string-comparison-punt
(defun std-char-lessp (char &rest others)
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ((code (char-code (char-upcase char))))
      (dolist (c others t)
        (unless (< code (setq code (char-code (char-upcase c))))
          (return))))))

#+string-comparison-punt
(defun std-char-greaterp (char &rest others)
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ((code (char-code (char-upcase char))))
      (dolist (c others t)
        (unless (> code (setq code (char-code (char-upcase c))))
          (return))))))

#+string-comparison-punt
(defun std-char-not-greaterp (char &rest others)
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ((code (char-code (char-upcase char))))
      (dolist (c others t)
        (when (> code (setq code (char-code (char-upcase c))))
          (return))))))

#+string-comparison-punt
(defun std-char-not-lessp (char &rest others)
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ((code (char-code (char-upcase char))))
      (dolist (c others t)
        (when (< code (setq code (char-code (char-upcase c))))
          (return))))))

;;; Following may cons more than they should
#+string-comparison-punt
(defun std-string-lessp (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (let ((upcase-string1 (string-upcase string1 :start start1 :end end1))
        (upcase-string2 (string-upcase string2 :start start2 :end end2)))
    (declare (dynamic-extent upcase-string1 upcase-string2))
    (string< upcase-string1 upcase-string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

#+string-comparison-punt
(defun std-string-greaterp (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (let ((upcase-string1 (string-upcase string1 :start start1 :end end1))
        (upcase-string2 (string-upcase string2 :start start2 :end end2)))
    (declare (dynamic-extent upcase-string1 upcase-string2))
    (string> upcase-string1 upcase-string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

#+string-comparison-punt
(defun std-string-not-greaterp (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (let ((upcase-string1 (string-upcase string1 :start start1 :end end1))
        (upcase-string2 (string-upcase string2 :start start2 :end end2)))
    (declare (dynamic-extent upcase-string1 upcase-string2))
    (string<= upcase-string1 upcase-string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

#+string-comparison-punt
(defun std-string-not-lessp (string1 string2 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (let ((upcase-string1 (string-upcase string1 :start start1 :end end1))
        (upcase-string2 (string-upcase string2 :start start2 :end end2)))
    (declare (dynamic-extent upcase-string1 upcase-string2))
    (string>= upcase-string1 upcase-string2 :start1 start1 :end1 end1 :start2 start2 :end2 end2)))

;;; Don't use macros here because these things sometimes need to be passed as funargs
#-string-comparison-punt
(eval-when (:compile :load :execute)
  (setf (fdefinition 'std-char-lessp)          (fdefinition 'char-lessp)
        (fdefinition 'std-char-greaterp)       (fdefinition 'char-greaterp)
        (fdefinition 'std-char-not-greaterp)   (fdefinition 'char-not-greaterp)
        (fdefinition 'std-char-not-lessp)      (fdefinition 'char-not-lessp)
        
        (fdefinition 'std-string-lessp)        (fdefinition 'string-lessp)
        (fdefinition 'std-string-greaterp)     (fdefinition 'string-greaterp)
        (fdefinition 'std-string-not-greaterp) (fdefinition 'string-not-greaterp)
        (fdefinition 'std-string-not-lessp)    (fdefinition 'string-not-lessp)))

;;; These definitions are unconditional because differences in implementation don't cause problems. See Note 1 above.
(eval-when (:compile :load :execute)
  (setf (fdefinition 'std-char-equal)       (fdefinition 'char-equal)
        (fdefinition 'std-char-not-equal)   (fdefinition 'char-not-equal)
        (fdefinition 'std-string-equal)     (fdefinition 'string-equal)
        (fdefinition 'std-string-not-equal) (fdefinition 'string-not-equal)))


