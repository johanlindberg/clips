(set-strategy depth)
(unwatch all)
; misclns1.bat test
(clear)
(open "misclns1.rsl" misclns1 "w")
(dribble-on "misclns1.out")
(batch "misclns1.bat")
(dribble-off)
(load "compline.clp")
(printout misclns1 "misclns1.bat differences are as follows:" crlf)
(compare-files misclns1.exp misclns1.out misclns1)
; close result file
(close misclns1)
