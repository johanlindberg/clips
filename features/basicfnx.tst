(unwatch all)
(clear)
(set-strategy depth)
(open "basicfnx.rsl" basicfnx "w")
(dribble-on "basicfnx.out")
(batch "basicfnx.bat")
(dribble-off)
(load "compline.clp")
(printout basicfnx "basicfnx.bat differences are as follows:" crlf)
(compare-files basicfnx.exp basicfnx.out basicfnx)
; close result file
(close basicfnx)
