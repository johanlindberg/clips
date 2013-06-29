(unwatch all)
(clear)
(set-strategy depth)
(open "iofnx.rsl" iofnx "w")
(dribble-on "iofnx.out")
(batch "iofnx.bat")
(dribble-off)
(load "compline.clp")
(printout iofnx "iofnx.bat differences are as follows:" crlf)
(compare-files iofnx.exp iofnx.out iofnx)
; close result file
(close iofnx)
