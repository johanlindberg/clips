(set-strategy depth)
(unwatch all)
; dynsal.clp test
(clear)
(open "dynsal.rsl" dynsal "w")
(load "compline.clp")
(load "dynsal.clp")
(progn (dribble-on "dynsal.out") (testit) (dribble-off))
(printout dynsal "dynsal.clp differences are as follows:" crlf)
(compare-files dynsal.exp dynsal.out dynsal)
; close result file
(close dynsal)
