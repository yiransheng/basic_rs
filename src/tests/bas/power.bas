10 REM POWER TABLE
11 DATA 8, 4
15 READ N0, P0
20 PRINT "N",
25 FOR P = 2 to P0
30   PRINT "N ^" P,
35 NEXT P
40 PRINT "SUM"
45 LET S = 0
50 FOR N = 2 TO N0
55   PRINT N,
60   FOR P = 2 TO P0
65     LET S = S + N ^ P
70     PRINT N ^ P,
75   NEXT P
80   PRINT S
85 NEXT N
99 END

-- out --
N              N ^2           N ^3           N ^4           SUM
2              4              8              16             28
3              9              27             81             145
4              16             64             256            481
5              25             125            625            1256
6              36             216            1296           2804
7              49             343            2401           5597
8              64             512            4096           10269
