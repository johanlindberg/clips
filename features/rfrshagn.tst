(set-strategy depth)
(unwatch all)
; rfrshagn.clp test
(clear)
(open "rfrshagn.rsl" rfrshagn "w")
(load "compline.clp")
(load "rfrshagn.clp")
(progn (dribble-on "rfrshagn.out") (testit) (dribble-off))
(printout rfrshagn "rfrshagn.clp differences are as follows:" crlf)
(compare-files rfrshagn.exp rfrshagn.out rfrshagn)
; close result file
(close rfrshagn)
