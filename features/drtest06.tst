(unwatch all)
(clear)
(set-strategy depth)
(open "drtest06.rsl" drtest06 "w")
(dribble-on "drtest06.out")
(batch "drtest06.bat")
(dribble-off)
(load "compline.clp")
(printout drtest06 "drtest06.bat differences are as follows:" crlf)
(compare-files drtest06.exp drtest06.out drtest06)
; close result file
(close drtest06)