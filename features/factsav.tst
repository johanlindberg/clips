(unwatch all)
(clear)
(dribble-on "factsav.out")
(batch "factsav.bat")
(dribble-off)
(clear)
(open "factsav.rsl" factsav "w")
(load "compline.clp")
(printout factsav "factsav.bat differences are as follows:" crlf)
(compare-files factsav.exp factsav.out factsav)
(close factsav)