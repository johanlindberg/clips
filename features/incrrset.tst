(set-strategy depth)
(unwatch all)
; incrrset.clp test
(clear)
(open "incrrset.rsl" incrrset "w")
(load "compline.clp")
(load "incrrset.clp")
(progn (dribble-on "incrrset.out") (testit) (dribble-off))
(printout incrrset "incrrset.clp differences are as follows:" crlf)
(compare-files incrrset.exp incrrset.out incrrset)
; close result file
(close incrrset)
