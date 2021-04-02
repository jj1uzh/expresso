(declare-const x String)
(declare-const y String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const x3 String)
(declare-const i Int)
(declare-const l Int)

(assert (and (<= 0 i) (< i (str.len x))))
(assert (and (<= 0 l) (<= (+ i l) (str.len x))))
(assert (= y (str.substr x i l)))
(assert (= x1 (str.reverse x)))
(assert (= x2 (str.substr x1 (- (- (str.len x) i) l) l)))
(assert (= x3 (str.reverse x2)))
(assert (not (= y x3)))

(check-sat)
(get-model)
