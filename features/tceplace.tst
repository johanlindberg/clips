(set-strategy depth)
(unwatch all)
; tceplace.bat test
(clear)
(open "tceplace.rsl" tceplace "w")
(load "compline.clp")
(dribble-on "tceplace.out")
(batch "tceplace.bat")
(dribble-off)
(printout tceplace "tceplace.bat differences are as follows:" crlf)
(compare-files tceplace.exp tceplace.out tceplace)
; close result file
(close tceplace)
