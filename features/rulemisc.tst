(set-strategy depth)
(unwatch all)
; rulemisc.bat test
(clear)
(open "rulemisc.rsl" rulemisc "w")
(dribble-on "rulemisc.out")
(batch "rulemisc.bat")
(dribble-off)
(load "compline.clp")
(printout rulemisc "rulemisc.bat differences are as follows:" crlf)
(compare-files rulemisc.exp rulemisc.out rulemisc)
; close result file
(close rulemisc)
