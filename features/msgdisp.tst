(unwatch all)
(clear)
(load "msgdisp.clp")
(dribble-on "msgdisp.out")
(testit)
(dribble-off)
(clear)
(open "msgdisp.rsl" msgdisp "w")
(load "compline.clp")
(printout msgdisp "msgdisp.clp differences are as follows:" crlf)
(compare-files msgdisp.exp msgdisp.out msgdisp)
(close msgdisp)