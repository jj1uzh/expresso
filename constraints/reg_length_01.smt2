(declare-const x String)
(declare-const y String)
(declare-const xy String)

(assert (= xy (str.++ x y)))
(assert (str.in.re xy (str.to.re "a")))

(check-sat)
(get-model)
