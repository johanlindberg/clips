(unwatch all)
(clear)
(set-strategy depth)
(open "drtest02.rsl" drtest02 "w")
(dribble-on "drtest02.out")
(batch "drtest02.bat")
(dribble-off)
(load "compline.clp")
(printout drtest02 "drtest02.bat differences are as follows:" crlf)
(compare-files drtest02.exp drtest02.out drtest02)
; close result file
(close drtest02)
