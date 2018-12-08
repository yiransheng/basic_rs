10 REM comments
20 GOTO 40
30 REM unreachable
40 REM should not break GOTO
50 PRINT "OK"
60 END

-- out --
OK
