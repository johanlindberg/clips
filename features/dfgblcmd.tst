(unwatch all)
(clear)
(dribble-on "dfgblcmd.out")
(batch "dfgblcmd.bat")
(dribble-off)
(clear)
(open "dfgblcmd.rsl" dfgblcmd "w")
(load "compline.clp")
(printout dfgblcmd "dfgblcmd.bat differences are as follows:" crlf)
(compare-files dfgblcmd.exp dfgblcmd.out dfgblcmd)
(close dfgblcmd)
