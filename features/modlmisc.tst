(set-strategy depth)
(unwatch all)
; modlmisc.bat test
(clear)
(open "modlmisc.rsl" modlmisc "w")
(dribble-on "modlmisc.out")
(batch "modlmisc.bat")
(dribble-off)
(load "compline.clp")
(printout modlmisc "modlmisc.bat differences are as follows:" crlf)
(compare-files modlmisc.exp modlmisc.out modlmisc)
; close result file
(close modlmisc)
