(unwatch all)
(clear)
(dribble-on "modulprt.out")
(batch "modulprt.bat")
(dribble-off)
(clear)
(open "modulprt.rsl" modulprt "w")
(load "compline.clp")
(printout modulprt "modulprt.bat differences are as follows:" crlf)
(compare-files modulprt.exp modulprt.out modulprt)
(close modulprt)