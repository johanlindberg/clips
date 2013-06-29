(set-strategy depth)
(unwatch all)
; mfvmatch.clp test
(clear)
(load "mfvmatch.clp")
(progn (dribble-on "mfvmatch.out") (reset) (run) (dribble-off))
(load compline.clp)
(open "mfvmatch.rsl" mfvmatch "w")
(printout mfvmatch "mfvmatch.clp differences are as follows:" crlf)
(compare-files mfvmatch.exp mfvmatch.out mfvmatch)
; close result file
(close mfvmatch)
