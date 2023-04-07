;; -*- mode: lisp -*-
;; L = { 0^i.1^j | i >= j }
(nonreg/original
 w
 (assert (str.in.re w (re.++ (re.* (str.to.re "0")) (re.* (str.to.re "1")))))
 (assert (>= (str.count_char w "0") (str.count_char w "1"))))

;; pumping lemma
;; wp = 0^p.1^p
(nonreg/subset
 p w
 (assert (= p (str.count_char w "0")))
 (assert (= p (str.count_char w "1"))))

(nonreg/split i j x y z)

(nonreg/evidence
 0 w0
 (assert-evidence (>= (str.count_char w0 "0") (str.count_char w0 "1"))))
