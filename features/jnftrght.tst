(set-strategy depth)
(unwatch all)
; jnftrght.bat test
(clear)
(open "jnftrght.rsl" jnftrght "w")
(dribble-on "jnftrght.out")
(batch "jnftrght.bat")
(dribble-off)
(load "compline.clp")
(printout jnftrght "jnftrght.clp differences are as follows:" crlf)
(compare-files jnftrght.exp jnftrght.out jnftrght)
; close result file
(close jnftrght)
