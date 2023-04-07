;; -*- mode:lisp -*-
; L = { w | w = 0^n.1^n }
(nonreg/original
 w
 (declare-const x_1 String)
 (declare-const x_2 String)
 (declare-const n Int)
 (assert (= w (str.++ x_1 x_2)))
 (assert (str.in.re x_1 (re.parse "0*")))
 (assert (str.in.re x_2 (re.parse "1*")))
 (assert (= (str.len x_1) n))
 (assert (= (str.len x_2) n)))

;; (nonreg/subset
;;  p wp
;;  (declare-const px_1 String)
;;  (declare-const px_2 String)
;;  (assert (= wp (str.++ px_1 px_2)))
;;  (assert (str.in.re px_1 (re.parse "0*")))
;;  (assert (str.in.re px_2 (re.parse "1*")))
;;  (assert (= (str.len px_1) p))
;;  (assert (= (str.len px_2) p)))
(nonreg/subset
 p w
 (assert (= n p)))

(nonreg/split i j x y z)

(nonreg/evidence
 2 w0
 (assert-evidence (= (str.count_char w0 "0") (str.count_char w0 "1"))))
;; (assert (< (str.count_char w0 "0") (str.count_char w0 "1"))))
