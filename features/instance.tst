(unwatch all)
(clear)
(setgen 1)
(load "instance.clp")
(dribble-on "instance.out")
(batch "instance.bat")
(dribble-off)
(clear)
(open "instance.rsl" instance "w")
(load "compline.clp")
(printout instance "instance.clp differences are as follows:" crlf)
(compare-files instance.exp instance.out instance)
(close instance)
