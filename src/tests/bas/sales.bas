10  FOR I = 1 TO 3
20  READ P(I)
30  NEXT I
40  FOR I = 1 TO 3
50  FOR J = 1 TO 5
60  READ S(I, J)
70  NEXT J
80  NEXT I
90  FOR J = 1 TO 5
100 LET S = 0
110 FOR I = 1 TO 3
120 LET S = S + P(I) * S(I,J)
130 NEXT I
140 PRINT "TOTAL SALES FOR SALESMAN "J, "$"S
150 NEXT J
200 DATA 1.25, 4.30, 2.50
210 DATA 40, 20, 37, 29, 42
220 DATA 10, 16,  3, 21,  8
230 DATA 35, 47, 29, 16, 33
300 END

-- out --
TOTAL SALES FOR SALESMAN 1    $180.5
TOTAL SALES FOR SALESMAN 2    $211.3
TOTAL SALES FOR SALESMAN 3    $131.65
TOTAL SALES FOR SALESMAN 4    $166.55
TOTAL SALES FOR SALESMAN 5    $169.4
