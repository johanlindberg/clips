(unwatch all)
(clear)
(set-strategy depth)
(open "mathfnx.rsl" mathfnx "w")
(dribble-on "mathfnx.out")
(batch "mathfnx.bat")
(dribble-off)
(load "compline.clp")
(printout mathfnx "mathfnx.bat differences are as follows:" crlf)
(compare-files mathfnx.exp mathfnx.out mathfnx)
; close result file
(close mathfnx)