(unwatch all)
(clear)
(set-static-constraint-checking TRUE)
(dribble-on "stobjcst.out")
(batch "stobjcst.bat")
(dribble-off)
(clear)
(open "stobjcst.rsl" stobjcst "w")
(load "compline.clp")
(printout stobjcst "stobjcst.clp differences are as follows:" crlf)
(compare-files stobjcst.exp stobjcst.out stobjcst)
(close stobjcst)
