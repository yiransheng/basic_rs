10  REM Source: http://nbviewer.jupyter.org/github/norvig/pytudes/blob/master/ipynb/BASIC.ipynb
20  REM By: Peter Norvig
100 REM CONWAY'S GAME OF LIFE

102 REM G IS NUMBER OF GENERATIONS, 
104 REM M IS MATRIX SIZE (M X M)
106 REM L(SELF, NEIGHBORS_ALIVE) IS 1 IFF CELL WITH THOSE COUNTS LIVES
108 REM A(X, Y) IS 1 IFF CELL AT (X, Y) IS LIVE
110 REM B(X, Y) GETS THE NEXT GENERATION

111 REM Added from Norvig's version: http://nbviewer.jupyter.org/github/norvig/pytudes/blob/master/ipynb/BASIC.ipynb
112 REM Python implementation above do not check/enforce array dimension limits
113 REM Default 11x11 is not enough for this program (20 is arbitrarily chosen)
115 DIM A(20, 20)

120 READ G,      M,      L(0,3), L(1,3), L(1,2)
121 DATA 10,     10,     1,      1,      1
130 READ A(3,4), A(3,5), A(3,6), A(6,5), A(6,6), A(7,5), A(7,6)
131 DATA 1,      1,      1,      1,      1,      1,      1

150 REM MAIN LOOP: PRINT, THEN REPEAT G TIMES: UPDATE / COPY / PRINT
155 LET I = 0
160 GOSUB 700
170 FOR I = 1 TO G
180   GOSUB 300
190   GOSUB 500
200   GOSUB 700
210 NEXT I
220 STOP

300 REM ========== UPDATE B = NEXT_GEN(A)
310 FOR Y = 1 TO M
320   FOR X = 1 TO M
325     LET N = A(X-1,Y)+A(X+1,Y)+A(X,Y-1)+A(X,Y+1)+A(X-1,Y-1)+A(X+1,Y+1)+A(X-1,Y+1)+A(X+1,Y-1)
330     LET B(X, Y) = L(A(X, Y), N)
340   NEXT X
350 NEXT Y
360 RETURN

500 REM ========== COPY A = B
510 FOR Y = 1 TO M
520   FOR X = 1 TO M
530     LET A(X, Y) = B(X, Y)
540   NEXT X
550 NEXT Y
560 RETURN

700 REM ========== PRINT A
705 PRINT "GEN " I
710 FOR Y = 1 TO M
720   FOR X = 1 TO M
730     IF A(X, Y) = 1 THEN 760
740       PRINT ".";
750       GOTO 770
760     PRINT "O";
770   NEXT X
780   PRINT
790 NEXT Y
795 RETURN

999 END 
