10 REM TEST REDEFINE A FUNCTION
15 DEF FNZ(X) = X
20 DEF FNA(X) = 1 + X
25 LET Y1 = FNA(10)
30 PRINT "FNA(10) ="; Y1
40 DEF FNA(X) = FNZ(X) - 1
45 LET Y2 = FNA(10)
50 PRINT "FNA(10) ="; Y2
60 IF Y1 <> Y2 THEN 100
70 PRINT "FAILED"
80 END
100 PRINT "Ok"
110 END
