10 FOR I = 0 TO 5
20   REM noop
30 NEXT I
40 PRINT "assert I > TO, TO = 5", "I=", I
50 FOR J = 10 TO 0
60   PRINT "Unreachable"
70 NEXT J
90 FOR I = 10 TO 1 STEP -6
100 NEXT I
110 PRINT "assert I < TO, TO = 1", "I=", I
120 END

-- out --
assert I > TO, TO = 5         I=             6
assert I < TO, TO = 1         I=             -2
