10 REM Inf loop, but exits after no data
20 FOR I = 1 TO 10 STEP 0
30   READ A, B, X(1, 1)
35   PRINT "A =", A, "B=", B
40 NEXT I
50 DATA 1, 2, 5
60 DATA 2, 3, 10 

-- out --
A =            1              B=             2
A =            2              B=             3

