(unwatch all)
(clear)
(dribble-on "factscmd.out")
(batch "factscmd.bat")
(dribble-off)
(clear)
(open "factscmd.rsl" factscmd "w")
(load "compline.clp")
(printout factscmd "factscmd.bat differences are as follows:" crlf)
(compare-files factscmd.exp factscmd.out factscmd)
(close factscmd)