10 REM Monte Carlo PI
110 LET C = 0
120 REM LET N = 1000
230 FOR I = 1 TO 1000
240   LET X = RND(X)
250   LET Y = RND(X)
260   LET D = X * X + Y * Y
270   IF D > 1 THEN 290
280   LET C = C + 1
290 NEXT I
300 LET P = 4 * C / 1000
310 PRINT "PI =", P, C"/"1000
320 END
