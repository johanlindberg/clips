(unwatch all)
(clear)
(set-sequence-operator-recognition FALSE)
(setgen 1)
(dribble-on "seqop.out")
(batch "seqop.bat")
(dribble-off)
(set-sequence-operator-recognition FALSE)
(clear)
(open "seqop.rsl" seqop "w")
(load "compline.clp")
(printout seqop "seqop.bat differences are as follows:" crlf)
(compare-files seqop.exp seqop.out seqop)
(close seqop)