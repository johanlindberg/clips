(unwatch all)
(clear)
(dribble-on "visible.out")
(load "visible.clp")
(go)
(dribble-off)
(clear)
(open "visible.rsl" visible "w")
(load "compline.clp")
(printout visible "visible.clp differences are as follows:" crlf)
(compare-files visible.exp visible.out visible)
(close visible)
