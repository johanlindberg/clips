(set-strategy depth)
(unwatch all)
; lgclexe.clp test
(clear)
(open "lgclexe.rsl" lgclexe "w")
(load "compline.clp")
(load "lgclexe.clp")
(progn (dribble-on "lgclexe.out") (test-logical) (dribble-off))
(printout lgclexe "lgclexe.clp differences are as follows:" crlf)
(compare-files lgclexe.exp lgclexe.out lgclexe)
; close result file
(close lgclexe)
