(set-strategy depth)
(unwatch all)
; misclns3.bat test
(clear)
(open "misclns3.rsl" misclns3 "w")
(dribble-on "misclns3.out")
(batch "misclns3.bat")
(dribble-off)
(load "compline.clp")
(printout misclns3 "misclns3.bat differences are as follows:" crlf)
(compare-files misclns3.exp misclns3.out misclns3)
; close result file
(close misclns3)
