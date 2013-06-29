(unwatch all)
(clear)
(set-strategy depth)
(open "multifnx.rsl" multifnx "w")
(dribble-on "multifnx.out")
(batch "multifnx.bat")
(dribble-off)
(load "compline.clp")
(printout multifnx "multifnx.bat differences are as follows:" crlf)
(compare-files multifnx.exp multifnx.out multifnx)
; close result file
(close multifnx)
