(set-strategy depth)
(unwatch all)
; conres.clp test
(clear)
(open "conres.rsl" conres "w")
(load "compline.clp")
(load "conres.clp")
(progn (dribble-on "conres.out") (testit) (dribble-off))
(printout conres "conres.clp differences are as follows:" crlf)
(compare-files conres.exp conres.out conres)
; close result file
(close conres)
