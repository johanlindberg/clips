(set-strategy depth)
(unwatch all)
; smfmmix.clp test
(clear)
(open "sfmfmix.rsl" sfmfmix "w")
(load "compline.clp")
(dribble-on "sfmfmix.out")
(load "sfmfmix.clp")
(rules)
(dribble-off)
(printout sfmfmix "sfmfmix.clp differences are as follows:" crlf)
(compare-files sfmfmix.exp sfmfmix.out sfmfmix)
; close result file
(close sfmfmix)
