(unwatch all)
; globlerr.clp test
(clear)
(open "globlerr.rsl" globlerr "w")
(load "compline.clp")
(dribble-on "globlerr.out")
(load "globlerr.clp")
(show-defglobals)
(dribble-off)
(printout globlerr "globlerr.clp differences are as follows:" crlf)
(compare-files globlerr.exp globlerr.out globlerr)
; close result file
(close globlerr)
