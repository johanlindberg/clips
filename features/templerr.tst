(unwatch all)
; templerr.clp test
(clear)
(open "templerr.rsl" templerr "w")
(load "compline.clp")
(dribble-on "templerr.out")
(load "templerr.clp")
(list-deftemplates)
(dribble-off)
(printout templerr "templerr.clp differences are as follows:" crlf)
(compare-files templerr.exp templerr.out templerr)
; close result file
(close templerr)
