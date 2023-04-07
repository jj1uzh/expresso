;; -*- mode:lisp -*-
(nonreg/original
 v1
 (declare-const v0 String)
 (assert (= v1 (str.++ v0 v0)))
; (assert (str.in.re v0 (re.parse "(0|1)*")))
 (assert (str.in.re v0 (re.* (re.union (str.to.re "0") (str.to.re "0"))))))

(nonreg/subset
 p v1
; (assert (str.in.re v0 (re.parse "0*1")))
 (assert (str.in.re v0 (re.++ (re.* (str.to.re "0")) (str.to.re "1"))))
 (assert (= (str.len v0) (+ p 1))))

(nonreg/split i j x y z)
;(nonreg/split ix iu iy iv iz x u y v z)

(nonreg/evidence
 0 w0
 (declare-const k Int)
 (declare-const l Int)
 (assert (= k (str.indexof w0 "1" 0)))
 (assert (= l (str.indexof w0 "1" (+ 1 k))))
; (assert-evidence (or (str.in.re w0 (re.comp (re.parse "0*10*1"))) (= l (+ 1 (* 2 k)))))
 (assert-evidence (or (str.in.re w0 (re.comp (re.++ (re.* (str.to.re "0")) (str.to.re "1") (re.* (str.to.re "0")) (str.to.re "1")))) (= l (+ 1 (* 2 k))))))
