(unwatch all)
(clear)
(set-strategy depth)
(open "strngfnx.rsl" strngfnx "w")
(dribble-on "strngfnx.out")
(batch "strngfnx.bat")
(dribble-off)
(load "compline.clp")
(printout strngfnx "strngfnx.bat differences are as follows:" crlf)
(compare-files strngfnx.exp strngfnx.out strngfnx)
; close result file
(close strngfnx)
