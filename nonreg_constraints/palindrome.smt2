;; -*- mode:lisp -*-
;; L = { w = rev(w) }
(nonreg/original
 w
 (declare-const w_pre String)
 (declare-const w_mid String)
 (declare-const w_pos String)
 (assert (= w (str.++ w_pre w_mid w_pos)))
 (assert (= w_pos (str.reverse w_pre)))
 (assert (str.in.re w_pre (re.parse "(0|1)*")))
 (assert (str.in.re w_mid (re.parse "|0|1"))))

(nonreg/subset
 p w
 (assert (str.in.re w_pre (re.parse "0*")))
 (assert (= (str.len w_pre) p))
 (assert (= w_mid "1")))

(nonreg/split
 i j x y z)

(nonreg/evidence
 0 w0
 (declare-const one-idx Int)
 (assert (= one-idx (str.indexof w0 "1" 0)))
 (assert-evidence (or (str.in.re w0 (re.comp (re.parse "0*10*"))) (= (* 2 one-idx) (- (str.len w0) 1)))))

;; (declare-const w String)

;; ;; target: w
;; (declare-const w_pre String)
;; (declare-const w_mid String)
;; (declare-const w_post String)
;; (assert (= w_post (str.reverse w_pre)))
;; (assert (= w (str.++ w_pre w_mid w_post)))
;; (assert (str.in.re w_pre (re.parse "(0|1)*")))
;; (assert (str.in.re w_post (re.parse "(0|1)*")))
;; (assert (str.in.re w_mid (re.parse "|0|1")))

;; ;; pumping lemma
;; (declare-const p Int)
;; (assert (str.in.re w (re.parse "0*110*")))

;; (declare-const i Int)
;; (declare-const j Int)
;; (declare-const w1 String)
;; (declare-const w2 String)
;; (declare-const w3 String)
;; (assert (>= i 0))
;; (assert (> j 0))
;; (assert (<= (+ i j) p))
;; (assert (= w1 (str.substr pw 0 i)))
;; (assert (= w2 (str.substr pw i j)))
;; (assert (= w3 (str.substr pw (+ i j) (- (- (str.len pw) i) j))))

;; (declare-const wp String)
;; (assert (= wp (str.++ w1 w3)))
;; (assert (or (str.in.re wp (re.comp (re.parse "0*110*"))) (= (str.len wp) (+ 1 (str.indexof wp "11" 0)))))

;; (check-sat)
;; (get-model)
