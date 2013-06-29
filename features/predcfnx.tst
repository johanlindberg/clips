(unwatch all)
(clear)
(set-strategy depth)
(open "predcfnx.rsl" predcfnx "w")
(dribble-on "predcfnx.out")
(batch "predcfnx.bat")
(dribble-off)
(load "compline.clp")
(printout predcfnx "predcfnx.bat differences are as follows:" crlf)
(compare-files predcfnx.exp predcfnx.out predcfnx)
; close result file
(close predcfnx)
