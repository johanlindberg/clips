(unwatch all)
(watch compilations)
(clear)
(dribble-on "joinshre.out")
(load "joinshre.clp")
(reset)
(agenda)
(assert (a-1) (a-2) (a-3) (a-4) (a-5))
(agenda)
(assert (d-1) (e-1) (f-1) (b-2) (b-3) (b-4) (b-5))
(agenda)
(assert (b-1) (c-2) (c-3) (c-4) (c-5))
(agenda)
(assert (c-1) (d-2) (d-3) (d-5))
(agenda)
(assert (e-5))
(agenda)
(assert (f-5) (g-5))
(agenda)
(dribble-off)
(unwatch compilations)
(clear)
(open "joinshre.rsl" joinshre "w")
(load "compline.clp")
(printout joinshre "joinshre.clp differences are as follows:" crlf)
(compare-files joinshre.exp joinshre.out joinshre)
(close joinshre)