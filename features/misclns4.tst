(set-strategy depth)
(unwatch all)
; misclns4.bat test
(clear)
(open "misclns4.rsl" misclns4 "w")
(dribble-on "misclns4.out")
(batch "misclns4.bat")
(dribble-off)
(load "compline.clp")
(printout misclns4 "misclns4.bat differences are as follows:" crlf)
(compare-files misclns4.exp misclns4.out misclns4)
; close result file
(close misclns4)
