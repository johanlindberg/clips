(set-strategy depth)
(unwatch all)
; globltst.clp test
(clear)
(open "globltst.rsl" globltst "w")
(load "compline.clp")
(load "globltst.clp")
(reset)
(progn (dribble-on "globltst.out") (run) (dribble-off))
(printout globltst "globltst.clp differences are as follows:" crlf)
(compare-files globltst.exp globltst.out globltst)
; close result file
(close globltst)
